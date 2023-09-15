use anyhow::{Context, Result};
use std::sync::atomic::{AtomicU32, Ordering};
use winliner::{Optimizer, ProfileBuilder};

fn assert_optimize(
    optimizer: &Optimizer,
    // A map from `i`th call site to a list of (callee, count) pairs.
    profile: &[&[(u32, u32)]],
    input: &str,
    expected_wat: &str,
) -> Result<()> {
    let _ = env_logger::try_init();

    let mut builder = ProfileBuilder::new();
    for (call_site, calls) in profile.iter().enumerate() {
        let call_site = u32::try_from(call_site).unwrap();
        for (callee, count) in *calls {
            for _ in 0..*count {
                builder.add_indirect_call(*callee, call_site);
            }
        }
    }
    let profile = builder.build();

    let input = wat::parse_str(input).context("failed to parse test input as WAT")?;

    let actual_wasm = optimizer
        .optimize(&profile, &input)
        .context("failed to optimize input with given profile")?;

    if log::log_enabled!(log::Level::Debug) {
        static TEST_CASE: AtomicU32 = AtomicU32::new(0);
        let n = TEST_CASE.fetch_add(1, Ordering::AcqRel);
        log::debug!("Writing instrumented Wasm to `optimized{n}.wasm`");
        std::fs::write(format!("optimized{n}.wasm"), &actual_wasm).unwrap();
    }

    let actual_wat =
        wasmprinter::print_bytes(&actual_wasm).context("failed to print optimized Wasm as WAT")?;

    let actual_wat = actual_wat.trim();
    println!("Expected:\n\n{expected_wat}\n\n");
    let expected_wat = expected_wat.trim();
    println!("Actual:\n\n{actual_wat}\n\n");

    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        function_references: true,
        ..Default::default()
    })
    .validate_all(&actual_wasm)
    .context("the optimized wasm failed to validate")?;

    if expected_wat != actual_wat {
        panic!("expected != actual");
    }

    Ok(())
}

#[test]
fn basic() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1),
        &[&[(42, 1)]],
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const 36
  )
  (func (;1;) (type 1) (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 42) funcref (ref.func 0))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 36
  )
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 42
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 36
    else
      local.get 1
      call_indirect (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 42) funcref (ref.func 0))
)
        "#,
    )
}

// TODO FITZGEN: not enough total calls
#[test]
fn not_enough_total_calls() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(2),
        &[&[(42, 1)]],
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const 36
  )
  (func (;1;) (type 1) (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 42) funcref (ref.func 0))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 36
  )
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 42) funcref (ref.func 0))
)
        "#,
    )
}

// TODO FITZGEN: multiple call_indirect call sites inlined and not inlined

// TODO FITZGEN: no indirect calls

// TODO FITZGEN: multiple params and multiple returns

// TODO FITZGEN: multiple callees for same call site w/ high enough ratio that is the same and how
// we break ties

// TODO FITZGEN: not high enough ratio

// TODO FITZGEN: inline a call site with call sites that we want to inline inside it

// TODO FITZGEN: too much inline depth

// TODO FITZGEN: recursion

// TODO FITZGEN: inline function with multiple locals
