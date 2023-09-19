//! Instrumenting a Wasm program to observe indirect call callees.

use crate::cow_section::{borrowed, owned, CowSection};
use anyhow::{bail, ensure, Error, Result};
use std::str::FromStr;
use wasm_encoder::SectionId;
use wasmparser::{Chunk, Payload};

#[cfg(feature = "clap")]
use clap::Parser;

/// Instrument a Wasm binary to collect PGO data.
#[cfg_attr(feature = "clap", derive(Parser))]
pub struct Instrumenter {
    /// Allow `funcref` tables to be mutated.
    ///
    /// By default, Winliner will reject Wasm programs that mutate `funcref`
    /// tables, since that can lead to divergence between the original and
    /// winlined version of the program. This flag lets you pinky promise that
    /// actually it is okay in this particular case.
    #[cfg_attr(feature = "clap", clap(long))]
    allow_table_mutation: bool,

    /// Allow arbitrary table element offsets.
    ///
    /// By default, Winliner only allows constant table element offsets, so that
    /// it can determine exactly which function `table[N]` corresponds to. If
    /// Winliner doesn't definitively know which function `table[N]` is, it
    /// can't be sure that it is inlining the right function. This flag lets you
    /// pinky promise that a non-const offset table element isn't going to lead
    /// to divergence and misoptimization.
    #[cfg_attr(feature = "clap", clap(long))]
    allow_arbitrary_element_offsets: bool,

    /// Allow table imports.
    ///
    /// By default, Winliner only allows locally-defined tables, and disallows
    /// imported tables. This is because Winliner doesn't have any insight into
    /// the contents of imported tables, only the contents added from local
    /// element segments. This flag lets you pinky promise that an imported
    /// table isn't going to lead to divergence and misoptimization.
    #[cfg_attr(feature = "clap", clap(long))]
    allow_table_imports: bool,

    /// The strategy of instrumentation to use.
    ///
    /// Choices:
    ///
    /// * three-globals: A low-overhead but imprecise instrumentation strategy
    ///   that inserts three globals for every indirect call site.
    ///
    /// * host-calls: A precise but high-overhead strategy that inserts calls
    ///   out to the host.
    ///
    /// See the API documentation for `winliner::InstrumentationStrategy` for
    /// more details.
    #[cfg_attr(feature = "clap", clap(short, long, default_value = "three-globals"))]
    strategy: InstrumentationStrategy,
}

impl Default for Instrumenter {
    fn default() -> Self {
        Instrumenter {
            allow_table_mutation: false,
            allow_arbitrary_element_offsets: false,
            allow_table_imports: false,
            strategy: InstrumentationStrategy::ThreeGlobals,
        }
    }
}

impl Instrumenter {
    /// Construct a new `Instrumenter`.
    pub fn new() -> Self {
        Instrumenter::default()
    }

    /// Allow `funcref` tables to be mutated.
    ///
    /// By default, Winliner will reject Wasm programs that mutate `funcref`
    /// tables, since that can lead to divergence between the original and
    /// winlined version of the program. This method lets you pinky promise that
    /// actually it is okay in this particular case.
    pub fn allow_table_mutation(&mut self, allow: bool) -> &mut Self {
        self.allow_table_mutation = allow;
        self
    }

    /// Allow arbitrary table element offsets.
    ///
    /// By default, Winliner only allows constant table element offsets, so that
    /// it can determine exactly which function `table[N]` corresponds to. If
    /// Winliner doesn't definitively know which function `table[N]` is, it
    /// can't be sure that it is inlining the right function. This method lets
    /// you pinky promise that a non-const offset table element isn't going to
    /// lead to divergence and misoptimization.
    pub fn allow_arbitrary_element_offsets(&mut self, allow: bool) -> &mut Self {
        self.allow_arbitrary_element_offsets = allow;
        self
    }

    /// Allow table imports.
    ///
    /// By default, Winliner only allows locally-defined tables, and disallows
    /// imported tables. This is because Winliner doesn't have any insight into
    /// the contents of imported tables, only the contents added from local
    /// element segments. This method lets you pinky promise that an imported
    /// table isn't going to lead to divergence and misoptimization.
    pub fn allow_table_imports(&mut self, allow: bool) -> &mut Self {
        self.allow_table_imports = allow;
        self
    }

    /// Configure the instrumentation strategy.
    ///
    /// See the documentation for [`InstrumentationStrategy`] for details.
    pub fn strategy(&mut self, strategy: InstrumentationStrategy) -> &mut Self {
        self.strategy = strategy;
        self
    }

    /// Instrument the given Wasm binary.
    ///
    /// Returns a new version of the input Wasm binary that is instrumented to
    /// record PGO data.
    pub fn instrument(&self, wasm: &[u8]) -> Result<Vec<u8>> {
        let full_wasm = wasm;
        let mut wasm = wasm;
        let mut parser = wasmparser::Parser::new(0);
        let mut validator = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
            function_references: true,
            ..Default::default()
        });

        // The list of `wasm_encoder` sections we will join together as the new,
        // instrumented Wasm binary.
        let mut new_sections: Vec<CowSection> = vec![];

        // The index of the type for the host-call imported function.
        let mut host_call_type_index = None;
        // The index of the host-call imported function.
        let mut host_call_func_index = None;

        // The number of functions we have prepended to the function index
        // space, and need to shift all other function indices down by this
        // much.
        let num_prepended_funcs = match self.strategy {
            InstrumentationStrategy::ThreeGlobals => 0,
            InstrumentationStrategy::HostCalls => 1,
        };

        // The new global and export sections for when we are doing the
        // three-global strategy. We add entries to these as we see
        // `call_indirect` instructions, so these have to be long-lived and we
        // can't just add them to `new_sections` as we see the old global and
        // export sections (if any even exist).
        let (mut new_global_section, mut new_export_section) = match self.strategy {
            InstrumentationStrategy::ThreeGlobals => (
                Some(wasm_encoder::GlobalSection::new()),
                Some(wasm_encoder::ExportSection::new()),
            ),
            InstrumentationStrategy::HostCalls => (None, None),
        };

        // The new code section, containing the instrumented code.
        let mut new_code_section = Some(wasm_encoder::CodeSection::new());

        // The number of defined functions in this Wasm module.
        let mut defined_func_count = 0;
        // The defined function index we are currently processing.
        let mut current_defined_func = 0;

        // The number of parameters each type defines.
        let mut num_params_by_type = vec![];

        // The type index of each defined function.
        let mut defined_func_types = vec![];

        // The number of indirect call sites we've found.
        let mut num_indirect_call_sites = 0;

        // Helper to ensure we've added our modified type section when using the
        // host-calls strategy.
        let mut added_precise_host_types = false;
        let mut ensure_precise_host_type_section =
            |new_sections: &mut Vec<_>,
             types: Option<wasmparser::TypeSectionReader>,
             host_call_type_index: &mut Option<u32>|
             -> Result<()> {
                if added_precise_host_types {
                    return Ok(());
                }
                added_precise_host_types = true;
                owned(
                    new_sections,
                    precise_host_calls_new_type_section(types, host_call_type_index)?,
                );
                Ok(())
            };

        // Helper to ensure we've added our modified import section when using
        // the host-calls strategy.
        let mut added_precise_host_imports = false;
        let mut ensure_precise_host_imports_section =
            |new_sections: &mut Vec<_>,
             imports: Option<wasmparser::ImportSectionReader>,
             host_call_type_index: u32,
             host_call_func_index: &mut Option<u32>|
             -> Result<()> {
                if added_precise_host_imports {
                    return Ok(());
                }
                added_precise_host_imports = true;
                owned(
                    new_sections,
                    precise_host_calls_new_import_section(
                        imports,
                        host_call_type_index,
                        host_call_func_index,
                    )?,
                );
                Ok(())
            };

        loop {
            let (consumed, payload) = match parser.parse(wasm, /* eof = */ true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { consumed, payload } => (consumed, payload),
            };

            let mut sub_validator = validator.payload(&payload)?;
            match payload {
                Payload::Version { .. } => {}

                Payload::CustomSection(custom) => {
                    new_sections.push(CowSection::Borrowed(wasm_encoder::RawSection {
                        id: SectionId::Custom as _,
                        data: &full_wasm[custom.range()],
                    }))
                }

                Payload::TypeSection(types) => {
                    match self.strategy {
                        InstrumentationStrategy::ThreeGlobals => {
                            borrowed(&mut new_sections, full_wasm, types.clone(), SectionId::Type)
                        }
                        InstrumentationStrategy::HostCalls => {
                            ensure_precise_host_type_section(
                                &mut new_sections,
                                Some(types.clone()),
                                &mut host_call_type_index,
                            )?;
                        }
                    }
                    for ty in types.into_iter() {
                        let ty = ty?;
                        let num_params = match ty.structural_type {
                            wasmparser::StructuralType::Func(f) => f.params().len() as u32,
                            wasmparser::StructuralType::Array(_)
                            | wasmparser::StructuralType::Struct(_) => 0,
                        };
                        num_params_by_type.push(num_params);
                    }
                }

                Payload::ImportSection(imports) => {
                    for imp in imports.clone().into_iter() {
                        if let wasmparser::TypeRef::Table(_) = imp?.ty {
                            ensure!(
                                self.allow_table_imports,
                                "imported tables are disallowed and can lead to divergence between \
                                 the original and optimized Wasm programs",
                            );
                        }
                    }
                    match self.strategy {
                        InstrumentationStrategy::ThreeGlobals => {
                            borrowed(&mut new_sections, full_wasm, imports, SectionId::Import);
                        }
                        InstrumentationStrategy::HostCalls => {
                            ensure_precise_host_type_section(
                                &mut new_sections,
                                None,
                                &mut host_call_type_index,
                            )?;
                            ensure_precise_host_imports_section(
                                &mut new_sections,
                                Some(imports),
                                host_call_type_index.unwrap(),
                                &mut host_call_func_index,
                            )?;
                        }
                    }
                }

                Payload::FunctionSection(funcs) => {
                    if self.strategy == InstrumentationStrategy::HostCalls {
                        ensure_precise_host_type_section(
                            &mut new_sections,
                            None,
                            &mut host_call_type_index,
                        )?;
                        ensure_precise_host_imports_section(
                            &mut new_sections,
                            None,
                            host_call_type_index.unwrap(),
                            &mut host_call_func_index,
                        )?;
                    }
                    borrowed(
                        &mut new_sections,
                        full_wasm,
                        funcs.clone(),
                        SectionId::Function,
                    );
                    for ty_idx in funcs.into_iter() {
                        let ty_idx = ty_idx?;
                        defined_func_types.push(ty_idx);
                    }
                }

                Payload::TableSection(tables) => {
                    borrowed(&mut new_sections, full_wasm, tables, SectionId::Table)
                }

                Payload::MemorySection(memories) => {
                    borrowed(&mut new_sections, full_wasm, memories, SectionId::Memory)
                }

                Payload::TagSection(tags) => {
                    borrowed(&mut new_sections, full_wasm, tags, SectionId::Tag)
                }

                Payload::GlobalSection(globals) => match self.strategy {
                    InstrumentationStrategy::ThreeGlobals => {
                        let new_global_section = new_global_section.as_mut().unwrap();
                        for global in globals.into_iter() {
                            let global = global?;
                            new_global_section.global(
                                crate::convert::global_type(global.ty),
                                &crate::convert::const_expr(global.init_expr)?,
                            );
                        }
                    }
                    InstrumentationStrategy::HostCalls => {
                        let mut new_global_section = wasm_encoder::GlobalSection::new();
                        for global in globals.into_iter() {
                            let global = global?;

                            new_global_section.global(
                                crate::convert::global_type(global.ty),
                                &crate::convert::const_expr_with_func_delta(
                                    global.init_expr,
                                    num_prepended_funcs,
                                )?,
                            );
                        }
                        owned(&mut new_sections, new_global_section);
                    }
                },

                Payload::ExportSection(exports) => match self.strategy {
                    InstrumentationStrategy::ThreeGlobals => {
                        let new_export_section = new_export_section.as_mut().unwrap();
                        for export in exports.into_iter() {
                            let export = export?;
                            new_export_section.export(
                                export.name,
                                crate::convert::external_kind(export.kind),
                                export.index,
                            );
                        }
                    }
                    InstrumentationStrategy::HostCalls => {
                        borrowed(&mut new_sections, full_wasm, exports, SectionId::Export)
                    }
                },

                Payload::StartSection { func, range: _ } => owned(
                    &mut new_sections,
                    wasm_encoder::StartSection {
                        function_index: func + num_prepended_funcs,
                    },
                ),

                Payload::ElementSection(elements) => {
                    for elem in elements.clone().into_iter() {
                        let elem = elem?;
                        if let wasmparser::ElementKind::Active {
                            table_index: _,
                            offset_expr,
                        } = elem.kind
                        {
                            let mut ops = offset_expr.get_operators_reader().into_iter();
                            match (ops.next(), ops.next(), ops.next()) {
                                (
                                    Some(Ok(wasmparser::Operator::I32Const { .. })),
                                    Some(Ok(wasmparser::Operator::End)),
                                    None,
                                ) => {}
                                _ => ensure!(
                                    self.allow_arbitrary_element_offsets,
                                    "unsupported table element offset: only `i32.const N` offsets \
                                     are allowed"
                                ),
                            }
                        }
                    }

                    if num_prepended_funcs == 0 {
                        borrowed(&mut new_sections, full_wasm, elements, SectionId::Element);
                    } else {
                        owned(
                            &mut new_sections,
                            precise_host_calls_new_elements_section(num_prepended_funcs, elements)?,
                        );
                    }
                }

                Payload::DataCountSection { count, range: _ } => {
                    owned(&mut new_sections, wasm_encoder::DataCountSection { count })
                }

                Payload::CodeSectionStart { count, .. } => {
                    defined_func_count = count;
                }
                Payload::CodeSectionEntry(body) => {
                    match sub_validator {
                        wasmparser::ValidPayload::Func(validator, body) => {
                            let allocs = wasmparser::FuncValidatorAllocations::default();
                            let mut validator = validator.into_validator(allocs);
                            validator.validate(&body)?;
                            sub_validator = wasmparser::ValidPayload::Ok;
                        }
                        _ => unreachable!(),
                    }

                    let mut locals = body.get_locals_reader()?;
                    let mut new_locals = Vec::with_capacity(locals.get_count() as usize);
                    let mut num_locals = 0;
                    for _ in 0..locals.get_count() {
                        let (count, ty) = locals.read()?;
                        num_locals += count;
                        new_locals.push((count, crate::convert::val_type(ty)));
                    }

                    // Add a new temporary local for storing the current callee.
                    let num_params = num_params_by_type
                        [defined_func_types[current_defined_func as usize] as usize];
                    let current_callee_local = num_params + num_locals;
                    new_locals.push((1, wasm_encoder::ValType::I32));

                    let mut new_func = wasm_encoder::Function::new(new_locals);
                    let mut ops = body
                        .get_operators_reader()?
                        .into_iter_with_offsets()
                        .peekable();
                    while let Some(op_and_offset) = ops.next() {
                        let (op, offset) = op_and_offset?;

                        match op {
                            wasmparser::Operator::TableCopy { .. }
                            | wasmparser::Operator::TableSet { .. }
                            | wasmparser::Operator::TableFill { .. }
                            | wasmparser::Operator::TableInit { .. } => {
                                // TODO: Only do this check for funcref tables.
                                if !self.allow_table_mutation {
                                    bail!(
                                        "Found table mutation instruction at offset {offset}: {op:?}\n\
                                         \n\
                                         Table mutation is disallowed, since it makes winlining \n\
                                         unsound, and can lead to divergence between the original and\n\
                                         winlined version of the program."
                                    );
                                }
                            }

                            wasmparser::Operator::Call { function_index } => {
                                new_func.instruction(&wasm_encoder::Instruction::Call(
                                    function_index + num_prepended_funcs,
                                ));
                                continue;
                            }
                            wasmparser::Operator::RefFunc { function_index } => {
                                new_func.instruction(&wasm_encoder::Instruction::RefFunc(
                                    function_index + num_prepended_funcs,
                                ));
                                continue;
                            }

                            wasmparser::Operator::CallIndirect { .. } => {
                                let call_site_index = num_indirect_call_sites;
                                num_indirect_call_sites += 1;

                                match self.strategy {
                                    InstrumentationStrategy::ThreeGlobals => {
                                        let new_global_section =
                                            new_global_section.as_mut().unwrap();
                                        let new_export_section =
                                            new_export_section.as_mut().unwrap();

                                        let mut new_exported_global = |name: String, ty, init| {
                                            let global = new_global_section.len();
                                            new_global_section.global(ty, &init);
                                            new_export_section.export(
                                                name.as_str(),
                                                wasm_encoder::ExportKind::Global,
                                                global,
                                            );
                                            global
                                        };

                                        let call_site_total_global = new_exported_global(
                                            format!("__winliner_call_site_{call_site_index}_total"),
                                            wasm_encoder::GlobalType {
                                                val_type: wasm_encoder::ValType::I64,
                                                mutable: true,
                                            },
                                            wasm_encoder::ConstExpr::i64_const(0),
                                        );

                                        let last_callee_global = new_exported_global(
                                            format!("__winliner_call_site_{call_site_index}_last_callee"),
                                            wasm_encoder::GlobalType {
                                                val_type: wasm_encoder::ValType::I32,
                                                mutable: true,
                                            },
                                            wasm_encoder::ConstExpr::i32_const(-1),
                                        );

                                        let last_callee_count_global = new_exported_global(
                                            format!("__winliner_call_site_{call_site_index}_last_callee_count"),
                                            wasm_encoder::GlobalType {
                                                val_type: wasm_encoder::ValType::I64,
                                                mutable: true,
                                            },
                                            wasm_encoder::ConstExpr::i64_const(0),
                                        );

                                        // Emit the following code:
                                        //
                                        // ```wat
                                        // ;; $call_site_total += 1
                                        // global.get $call_site_total
                                        // i64.const 1
                                        // i64.add
                                        // global.set $call_site_total
                                        //
                                        // ;; if $last_callee != $current_callee {
                                        // ;;     $last_callee = $current_callee
                                        // ;;     $last_callee_count = 0
                                        // ;; }
                                        // local.tee $current_callee
                                        // global.get $last_callee
                                        // i32.eq
                                        // i32.eqz
                                        // if
                                        //     local.get $current_callee
                                        //     global.set $last_callee
                                        //     i64.const 0
                                        //     global.set $last_callee_count
                                        // end
                                        //
                                        // ;; $last_callee_count += 1
                                        // global.get $last_callee_count
                                        // i64.const 1
                                        // i64.add
                                        // global.set $last_callee_count
                                        //
                                        // ;; Finally, restore the operand stack for the indirect call.
                                        // local.get $current_callee
                                        // ```
                                        new_func
                                            .instruction(&wasm_encoder::Instruction::GlobalGet(
                                                call_site_total_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::I64Const(1))
                                            .instruction(&wasm_encoder::Instruction::I64Add)
                                            .instruction(&wasm_encoder::Instruction::GlobalSet(
                                                call_site_total_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::LocalTee(
                                                current_callee_local,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::GlobalGet(
                                                last_callee_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::I32Eq)
                                            .instruction(&wasm_encoder::Instruction::I32Eqz)
                                            .instruction(&wasm_encoder::Instruction::If(
                                                wasm_encoder::BlockType::Empty,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::LocalGet(
                                                current_callee_local,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::GlobalSet(
                                                last_callee_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::I64Const(0))
                                            .instruction(&wasm_encoder::Instruction::GlobalSet(
                                                last_callee_count_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::End)
                                            .instruction(&wasm_encoder::Instruction::GlobalGet(
                                                last_callee_count_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::I64Const(1))
                                            .instruction(&wasm_encoder::Instruction::I64Add)
                                            .instruction(&wasm_encoder::Instruction::GlobalSet(
                                                last_callee_count_global,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::LocalGet(
                                                current_callee_local,
                                            ));
                                    }
                                    InstrumentationStrategy::HostCalls => {
                                        // Emit the following code:
                                        //
                                        // ```
                                        // local.tee $current_callee
                                        // i32.const <call_site_index>
                                        // call $winliner_add_indirect_call
                                        // local.get $current_callee
                                        // ```
                                        new_func
                                            .instruction(&wasm_encoder::Instruction::LocalTee(
                                                current_callee_local,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::I32Const(
                                                call_site_index,
                                            ))
                                            .instruction(&wasm_encoder::Instruction::Call(
                                                host_call_func_index.unwrap(),
                                            ))
                                            .instruction(&wasm_encoder::Instruction::LocalGet(
                                                current_callee_local,
                                            ));
                                    }
                                }
                            }
                            _ => {}
                        }

                        let start = offset;

                        // Find the start of the next instruction, aka
                        // the end of this instruction, and copy over
                        // this instruction's raw bytes to the new
                        // function.
                        let end = ops
                            .peek()
                            .map_or(Ok(body.range().end), |res| res.clone().map(|(_, off)| off))?;

                        new_func.raw(full_wasm[start..end].iter().copied());
                    }

                    new_code_section.as_mut().unwrap().function(&new_func);
                    current_defined_func += 1;
                    if current_defined_func >= defined_func_count {
                        owned(&mut new_sections, new_code_section.take().unwrap());
                    }
                }

                Payload::DataSection(data) => {
                    borrowed(&mut new_sections, full_wasm, data, SectionId::Data)
                }

                Payload::ModuleSection { .. }
                | Payload::InstanceSection(_)
                | Payload::CoreTypeSection(_)
                | Payload::ComponentSection { .. }
                | Payload::ComponentInstanceSection(_)
                | Payload::ComponentAliasSection(_)
                | Payload::ComponentTypeSection(_)
                | Payload::ComponentCanonicalSection(_)
                | Payload::ComponentStartSection { .. }
                | Payload::ComponentImportSection(_)
                | Payload::ComponentExportSection(_) => {
                    unreachable!("component model not supported yet; disabled in validator")
                }

                Payload::UnknownSection {
                    id,
                    contents: _,
                    range,
                } => bail!("unknown section with id {id} at range {range:?}"),

                Payload::End(_) => break,
            }

            match sub_validator {
                wasmparser::ValidPayload::Ok | wasmparser::ValidPayload::End(_) => {
                    wasm = &wasm[consumed..];
                }
                _ => unreachable!(),
            }
        }

        log::trace!("Building final instrumented module");
        let mut module = wasm_encoder::Module::new();
        for section in &new_sections {
            use wasm_encoder::Section;

            if new_global_section
                .as_ref()
                .map_or(false, |s| s.id() < section.id())
            {
                let s = new_global_section.take().unwrap();
                log::trace!("Appending section id: {}", s.id());
                module.section(&s);
            }

            if new_export_section
                .as_ref()
                .map_or(false, |s| s.id() < section.id())
            {
                let s = new_export_section.take().unwrap();
                log::trace!("Appending section id: {}", s.id());
                module.section(&s);
            }

            log::trace!("Appending section id: {}", section.id());
            module.section(&*section);
        }
        Ok(module.finish())
    }
}

fn precise_host_calls_new_type_section(
    types: Option<wasmparser::TypeSectionReader>,
    host_call_type_index: &mut Option<u32>,
) -> Result<wasm_encoder::TypeSection> {
    let mut new_types = wasm_encoder::TypeSection::new();

    if let Some(types) = types {
        for ty in types.into_iter() {
            new_types.subtype(&crate::convert::sub_type(ty?));
        }
    }

    *host_call_type_index = Some(new_types.len());
    new_types.function([wasm_encoder::ValType::I32, wasm_encoder::ValType::I32], []);

    Ok(new_types)
}

fn precise_host_calls_new_import_section(
    imports: Option<wasmparser::ImportSectionReader>,
    host_call_type_index: u32,
    host_call_func_index: &mut Option<u32>,
) -> Result<wasm_encoder::ImportSection> {
    let mut new_imports = wasm_encoder::ImportSection::new();

    *host_call_func_index = Some(0);
    new_imports.import(
        "winliner",
        "add_indirect_call",
        wasm_encoder::EntityType::Function(host_call_type_index),
    );

    if let Some(imports) = imports {
        for import in imports.into_iter() {
            let import = import?;
            new_imports.import(
                import.module,
                import.name,
                crate::convert::type_ref(import.ty),
            );
        }
    }

    Ok(new_imports)
}

fn precise_host_calls_new_elements_section(
    num_prepended_funcs: u32,
    elements: wasmparser::ElementSectionReader,
) -> Result<wasm_encoder::ElementSection> {
    let mut new_elements = wasm_encoder::ElementSection::new();
    for elem in elements.into_iter() {
        let elem = elem?;

        let funcs;
        let exprs;
        let elements = match elem.items {
            wasmparser::ElementItems::Functions(items) => {
                funcs = items.into_iter().collect::<Result<Vec<_>, _>>()?;
                wasm_encoder::Elements::Functions(&funcs)
            }
            wasmparser::ElementItems::Expressions(ref_ty, items) => {
                exprs = items
                    .into_iter()
                    .map(|expr| {
                        let expr = expr?;
                        crate::convert::const_expr_with_func_delta(expr, num_prepended_funcs)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                wasm_encoder::Elements::Expressions(crate::convert::ref_type(ref_ty), &exprs)
            }
        };

        match elem.kind {
            wasmparser::ElementKind::Passive => {
                new_elements.passive(elements);
            }
            wasmparser::ElementKind::Active {
                table_index,
                offset_expr,
            } => {
                new_elements.active(
                    table_index,
                    &crate::convert::const_expr(offset_expr)?,
                    elements,
                );
            }
            wasmparser::ElementKind::Declared => {
                new_elements.declared(elements);
            }
        }
    }
    Ok(new_elements)
}

/// The instrumentation strategy for recording profiling data.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InstrumentationStrategy {
    /// A low-overhead but imprecise instrumentation strategy that records
    /// profiling information in globals.
    ///
    /// This strategy adds three globals per indirect call site:
    ///
    /// 1. The total number of calls for this call site.
    /// 2. The table index of the last indirect callee.
    /// 3. The number of times the last callee has been called.
    ///
    /// A `call_indirect` then becomes the following sequence:
    ///
    /// ```wat
    /// ;; $total_count += 1
    /// global.get $total_count
    /// i64.const 1
    /// i64.add
    /// global.set $total_count
    ///
    /// ;; if $last_callee != $current_callee {
    /// ;;     $last_callee = $current_callee
    /// ;;     $last_callee_count = 0
    /// ;; }
    /// local.tee $current_callee
    /// global.get $last_callee
    /// i32.eq
    /// i32.eqz
    /// if
    ///     local.get $current_callee
    ///     global.set $last_callee
    ///     i64.const 0
    ///     global.set $last_callee_count
    /// end
    ///
    /// ;; $last_callee_count += 1
    /// global.get $last_callee_count
    /// i64.const 1
    /// i64.add
    /// global.set $last_callee_count
    ///
    /// ;; Finally, do the actual indirect call.
    /// local.get $current_callee
    /// call_indirect
    /// ```
    ///
    /// When `$last_callee_count / $total_count` is high enough, then winlining
    /// `table[$last_callee]` is beneficial.
    ///
    /// Note that this strategy is imprecise and is easily defeated by the
    /// following sequence of calls:
    ///
    /// * Indirect call to `f`
    /// * Indirect call to `f`
    /// * Indirect call to `f`
    /// * ... many, many times ...
    /// * Indirect call to `f`
    /// * Indirect call to `g`
    ///
    /// In this case, winlining `f` would be beneficial, but we don't learn that
    /// because the last call to `g` clears that information away.
    ///
    /// However, this instrumentation's overhead is low enough that it is
    /// practical to run the instrumented Wasm programs in many production
    /// scenarios.
    ThreeGlobals,

    /// A precise but high-overhead strategy that inserts calls out to the host.
    ///
    /// This strategy inserts a call to an imported host function before every
    /// `call_indirect` instruction, letting the host record precise information
    /// about the number of indirect calls per call site and which function was
    /// the callee.
    ///
    /// The imported instrumentation function has the following module, name,
    /// and signature:
    ///
    /// ```wat
    /// (import "winliner" "add_indirect_call" (func (param i32 i32)))
    /// ```
    ///
    /// Each `call_indirect` is then transformed into the following sequence:
    ///
    /// ```wat
    /// ;; Call out to the host to record the indirect call.
    /// local.tee $current_callee
    /// i32.const 1234 ;; This is the 1234th indirect call site.
    /// call $winliner_add_indirect_call
    ///
    /// ;; Finally, do the actual indirect call.
    /// local.get $current_callee
    /// call_indirect
    /// ```
    ///
    /// When using this strategy, it is your responsibility to provide the host
    /// function that this instrumentation inserts calls to, and to build up the
    /// profiles using the [`ProfileBuilder`][crate::ProfileBuilder] type.
    ///
    /// Note that, while this strategy yields precise profiling information, it
    /// incurs fairly high overheads, likely making it unacceptable to run the
    /// instrumented Wasm programs in production scenarios.
    HostCalls,
}

impl FromStr for InstrumentationStrategy {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "three-globals" => Ok(InstrumentationStrategy::ThreeGlobals),
            "host-calls" => Ok(InstrumentationStrategy::HostCalls),
            _ => bail!(
                "Unknown instrumentation strategy '{s}'; valid strategies are: three-globals, \
                 host-calls"
            ),
        }
    }
}
