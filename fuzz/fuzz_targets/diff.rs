//! Differential fuzzing between original Wasm and optimized Wasm.
//!
//! Even given a bogus profile, speculative inlining should be semantically
//! transparent.

#![no_main]

use libfuzzer_sys::{
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};
use winliner::{Optimizer, Profile, ProfileBuilder};

const NUM_INPUTS: usize = 10;

#[derive(Debug)]
struct OptimizerConfig {
    min_total_calls: u64,
    min_ratio: f64,
    max_inline_depth: usize,
}

impl<'a> Arbitrary<'a> for OptimizerConfig {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let min_total_calls = u.int_in_range(0..=1_000)?;
        let min_ratio = u.arbitrary::<u8>()? as f64 / u8::MAX as f64;
        let max_inline_depth = u.int_in_range(0..=1_000)?;
        Ok(OptimizerConfig {
            min_total_calls,
            min_ratio,
            max_inline_depth,
        })
    }
}

#[derive(Debug)]
struct FuzzInput {
    module: wasm_smith::Module,
    profile: Vec<Vec<(u32, u32)>>,
    optimizer: OptimizerConfig,
    inputs: Vec<u128>,
}

impl<'a> Arbitrary<'a> for FuzzInput {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let profile = u.arbitrary()?;
        let optimizer = u.arbitrary()?;

        let mut inputs = Vec::with_capacity(NUM_INPUTS);
        for _ in 0..NUM_INPUTS {
            inputs.push(u.arbitrary()?);
        }

        let mut config: wasm_smith::SwarmConfig = u.arbitrary()?;

        // Don't generate imports, we have nothing for the Wasm to import.
        config.max_imports = 0;

        // Always generate at least one export.
        config.min_exports = 1;
        config.max_exports = config.max_exports.max(config.min_exports);

        // Always generate at least one type, since we will need it to define a
        // function and do `call_indirect`s.
        config.min_types = 1;
        config.max_types = config.max_types.max(config.min_types);

        // Always generate at least one table, since we will need it to do
        // `call_indirect`s.
        config.min_tables = 1;
        config.max_tables = config.max_tables.max(config.min_tables as _);

        // Always generate at least one function, since we will need it to do
        // differential execution.
        config.min_funcs = 1;
        config.max_funcs = config.max_funcs.max(config.min_funcs as _);

        // No `table.set`, etc... instructions.
        config.reference_types_enabled = false;

        let module = wasm_smith::Module::new(config, u)?;

        Ok(FuzzInput {
            module,
            profile,
            optimizer,
            inputs,
        })
    }
}

impl FuzzInput {
    fn profile(&self) -> Profile {
        let mut builder = ProfileBuilder::new();
        for (call_site, calls) in self.profile.iter().enumerate() {
            let call_site = u32::try_from(call_site).unwrap();
            for (callee, count) in calls {
                for _ in 0..*count {
                    builder.add_indirect_call(*callee, call_site);
                }
            }
        }
        builder.build()
    }

    fn optimizer(&self) -> Optimizer {
        let mut o = Optimizer::new();
        o.min_total_calls(self.optimizer.min_total_calls);
        o.min_ratio(self.optimizer.min_ratio).unwrap();
        o.max_inline_depth(self.optimizer.max_inline_depth);
        o
    }
}

fuzz_target!(|input: FuzzInput| {
    let original_wasm = input.module.to_bytes();
    let profile = input.profile();
    let optimizer = input.optimizer();

    let optimized_wasm = match optimizer.optimize(&profile, &original_wasm) {
        Ok(x) => x,
        Err(_) => return,
    };

    let mut config = wasmtime::Config::new();
    config.wasm_bulk_memory(true);
    config.wasm_reference_types(false);
    config.consume_fuel(true);

    let engine = wasmtime::Engine::new(&config).unwrap();

    // Compile both versions of the Wasm.
    let original_module = match wasmtime::Module::new(&engine, &original_wasm) {
        Ok(x) => x,
        Err(_) => return,
    };
    let optimized_module = match wasmtime::Module::new(&engine, &optimized_wasm) {
        Ok(x) => x,
        Err(_) => return,
    };

    // Find the first exported function.
    let (func_name, func_ty) = match original_module.exports().find_map(|exp| match exp.ty() {
        wasmtime::ExternType::Func(ty) => Some((exp.name(), ty)),
        _ => None,
    }) {
        Some(x) => x,
        None => return,
    };

    for i in 0..input.inputs.len() {
        // Build an arguments array from our fuzzer-chosen inputs.

        let mut args = Vec::with_capacity(func_ty.params().len());
        for (j, ty) in func_ty.params().enumerate() {
            let x = input.inputs[(i + j) % input.inputs.len()];
            args.push(match ty {
                wasmtime::ValType::I32 => wasmtime::Val::I32(x as _),
                wasmtime::ValType::I64 => wasmtime::Val::I64(x as _),
                wasmtime::ValType::F32 => wasmtime::Val::F32(x as _),
                wasmtime::ValType::F64 => wasmtime::Val::F64(x as _),
                wasmtime::ValType::V128 => wasmtime::Val::V128(x),
                wasmtime::ValType::FuncRef | wasmtime::ValType::ExternRef => return,
            });
        }

        // Call the original version of the function.

        let mut store = wasmtime::Store::new(&engine, ());
        store.add_fuel(1_000).unwrap();

        let instance = match wasmtime::Instance::new(&mut store, &original_module, &[]) {
            Ok(x) => x,
            Err(_) => return,
        };

        let func = instance.get_func(&mut store, func_name).unwrap();
        let mut original_results = vec![wasmtime::Val::I32(0); func_ty.results().len()];
        let original_result = func.call(&mut store, &args, &mut original_results);

        // Call the optimized version of the function.

        let mut store = wasmtime::Store::new(&engine, ());
        store.add_fuel(1_000).unwrap();

        let instance = match wasmtime::Instance::new(&mut store, &optimized_module, &[]) {
            Ok(x) => x,
            Err(_) => return,
        };

        let func = instance.get_func(&mut store, func_name).unwrap();
        let mut optimized_results = vec![wasmtime::Val::I32(0); func_ty.results().len()];
        let optimized_result = func.call(&mut store, &args, &mut optimized_results);

        // Check that both versions computed the "same" results.

        match (original_result.is_err(), optimized_result.is_err()) {
            // If both trapped, then continue to the next input.
            (true, true) => continue,

            // If they didn't both trap or both not trap, then this could just
            // be a difference of fuel accounting due to inlining vs not.
            (true, false) => assert!(original_result.unwrap_err().to_string().contains("fuel")),
            (false, true) => assert!(optimized_result.unwrap_err().to_string().contains("fuel")),

            // If neither trapped, then check that they both have the same
            // results.
            (false, false) => {
                for (original_result, optimized_result) in
                    original_results.into_iter().zip(optimized_results)
                {
                    match (original_result, optimized_result) {
                        (wasmtime::Val::I32(a), wasmtime::Val::I32(b)) => assert_eq!(a, b),
                        (wasmtime::Val::I64(a), wasmtime::Val::I64(b)) => assert_eq!(a, b),
                        (wasmtime::Val::F32(a), wasmtime::Val::F32(b)) => {
                            let a: f32 = unsafe { std::mem::transmute(a) };
                            let b: f32 = unsafe { std::mem::transmute(b) };
                            assert!(a == b || (a.is_nan() && b.is_nan()));
                        }
                        (wasmtime::Val::F64(a), wasmtime::Val::F64(b)) => {
                            let a: f64 = unsafe { std::mem::transmute(a) };
                            let b: f64 = unsafe { std::mem::transmute(b) };
                            assert!(a == b || (a.is_nan() && b.is_nan()));
                        }
                        (wasmtime::Val::ExternRef(_), wasmtime::Val::ExternRef(_)) => continue,
                        (wasmtime::Val::FuncRef(_), wasmtime::Val::FuncRef(_)) => continue,
                        _ => unreachable!("mismatched types"),
                    }
                }
            }
        }
    }
});
