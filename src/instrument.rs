//! Instrumenting a Wasm program to observe indirect call callees.

// TODO FITZGEN
#![allow(warnings)]

use anyhow::{Error, Result};
use std::str::FromStr;
use wasm_encoder::SectionId;
use wasmparser::{Chunk, Payload};

#[cfg(feature = "clap")]
use clap::Parser;

/// TODO FITZGEN
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

    /// The strategy of instrumentation to use.
    ///
    /// Choices:
    ///
    /// * three-globals: A low-overhead but imprecise instrumentation strategy
    ///   that inserts three globals for every indirect call site.
    ///
    /// * precise-host-calls: A precise but high-overhead strategy that inserts
    ///   calls out to the host.
    ///
    /// Default: three-globals
    #[cfg_attr(feature = "clap", clap(long))]
    strategy: InstrumentationStrategy,
}

impl Default for Instrumenter {
    fn default() -> Self {
        Instrumenter {
            allow_table_mutation: false,
            strategy: InstrumentationStrategy::ThreeGlobals,
        }
    }
}

impl Instrumenter {
    /// TODO FITZGEN
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

    /// The strategy of instrumentation to use.
    ///
    /// TODO FITZGEN
    pub fn strategy(&mut self, strategy: InstrumentationStrategy) -> &mut Self {
        self.strategy = strategy;
        self
    }

    /// TODO FITZGEN
    pub fn instrument(&self, wasm: &[u8]) -> Result<Vec<u8>> {
        let full_wasm = wasm;
        let mut wasm = wasm;
        let mut parser = wasmparser::Parser::new(0);
        let mut validator = wasmparser::Validator::new();

        // The list of `wasm_encoder` sections we will join together as the new,
        // instrumented Wasm binary.
        let mut new_sections: Vec<CowSection> = vec![];

        // The index of the type for the precise-host-call imported function.
        let mut host_call_type_index = None;

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
            InstrumentationStrategy::PreciseHostCalls => (None, None),
        };

        // The new code section, containing the instrumented code.
        let mut new_code_section = wasm_encoder::CodeSection::new();

        loop {
            let (consumed, payload) = match parser.parse(wasm, /* eof = */ true)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { consumed, payload } => (consumed, payload),
            };

            let sub_validator = validator.payload(&payload)?;
            match payload {
                Payload::Version { .. } => {}
                Payload::TypeSection(types) => match self.strategy {
                    InstrumentationStrategy::ThreeGlobals => {
                        new_sections.push(borrowed(full_wasm, types, SectionId::Type))
                    }
                    InstrumentationStrategy::PreciseHostCalls => {
                        let mut new_types = wasm_encoder::TypeSection::new();
                        for ty in types.into_iter() {
                            new_types.subtype(&crate::convert::sub_type(ty?));
                        }
                        host_call_type_index = Some(new_types.len());
                        new_types
                            .function([wasm_encoder::ValType::I32, wasm_encoder::ValType::I32], []);
                        new_sections.push(owned(new_types));
                    }
                },
                Payload::ImportSection(imports) => match self.strategy {
                    InstrumentationStrategy::ThreeGlobals => {
                        new_sections.push(borrowed(full_wasm, imports, SectionId::Import));
                    }
                    InstrumentationStrategy::PreciseHostCalls => {
                        let mut new_imports = wasm_encoder::ImportSection::new();
                        for import in imports.into_iter() {
                            let import = import?;
                            new_imports.import(
                                import.module,
                                import.name,
                                crate::convert::type_ref(import.ty),
                            );
                        }
                        new_imports.import(
                            "winliner",
                            "add_indirect_call",
                            wasm_encoder::EntityType::Function(host_call_type_index.unwrap()),
                        );
                        new_sections.push(owned(new_imports));
                    }
                },
                Payload::FunctionSection(funcs) => {
                    new_sections.push(borrowed(full_wasm, funcs, SectionId::Function))
                }
                Payload::TableSection(tables) => {
                    new_sections.push(borrowed(full_wasm, tables, SectionId::Table))
                }
                Payload::MemorySection(memories) => {
                    new_sections.push(borrowed(full_wasm, memories, SectionId::Memory))
                }
                Payload::TagSection(tags) => {
                    new_sections.push(borrowed(full_wasm, tags, SectionId::Tag))
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
                    InstrumentationStrategy::PreciseHostCalls => {
                        new_sections.push(borrowed(full_wasm, globals, SectionId::Global))
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
                    InstrumentationStrategy::PreciseHostCalls => {
                        new_sections.push(borrowed(full_wasm, exports, SectionId::Export))
                    }
                },
                Payload::StartSection { func, range: _ } => {
                    new_sections.push(owned(wasm_encoder::StartSection {
                        function_index: func,
                    }))
                }
                Payload::ElementSection(elements) => {
                    new_sections.push(borrowed(full_wasm, elements, SectionId::Element))
                }
                Payload::DataCountSection { count, range: _ } => {
                    new_sections.push(owned(wasm_encoder::DataCountSection { count }))
                }
                Payload::DataSection(data) => {
                    new_sections.push(borrowed(full_wasm, data, SectionId::Data))
                }
                Payload::CodeSectionStart { count, range, size } => {}
                Payload::CodeSectionEntry(body) => {
                    let mut locals = body.get_locals_reader()?;
                    let mut new_locals = Vec::with_capacity(locals.get_count() as usize);
                    let mut num_locals = 0;
                    for _ in 0..locals.get_count() {
                        let (count, ty) = locals.read()?;
                        num_locals += count;
                        new_locals.push((count, crate::convert::val_type(ty)));
                    }

                    // Add a new temporary local for storing the current callee.
                    let current_callee_local = num_locals;
                    new_locals.push((1, wasm_encoder::ValType::I32));

                    let mut new_func = wasm_encoder::Function::new(new_locals);
                    let mut ops = body
                        .get_operators_reader()?
                        .into_iter_with_offsets()
                        .peekable();
                    while let Some(op) = ops.next() {
                        match op? {
                            (
                                wasmparser::Operator::CallIndirect {
                                    type_index,
                                    table_index,
                                    table_byte,
                                },
                                _offset,
                            ) => match self.strategy {
                                InstrumentationStrategy::ThreeGlobals => todo!("TODO FITZGEN"),
                                InstrumentationStrategy::PreciseHostCalls => todo!("TODO FITZGEN"),
                            },
                            (_op, start) => {
                                // Find the start of the next instruction, aka
                                // the end of this instruction, and copy over
                                // this instruction's raw bytes to the new
                                // function.
                                let end = ops.peek().map_or(Ok(full_wasm.len()), |res| {
                                    res.clone().map(|(_, off)| off)
                                })?;
                                new_func.raw(full_wasm[start..end].iter().copied());
                            }
                        }
                    }
                }

                Payload::CustomSection(custom) => {
                    new_sections.push(CowSection::Borrowed(wasm_encoder::RawSection {
                        id: SectionId::Custom as _,
                        data: &full_wasm[custom.range()],
                    }))
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
                    contents,
                    range,
                } => anyhow::bail!("unknown section with id {id} at range {range:?}"),

                Payload::End(_) => break,
            }

            wasm = &wasm[consumed..];
        }

        let mut module = wasm_encoder::Module::new();
        for section in &new_sections {
            // TODO FITZGEN: maybe insert new export and global sections.
            module.section(&*section);
        }
        Ok(module.finish())
    }
}

enum CowSection<'a> {
    Borrowed(wasm_encoder::RawSection<'a>),
    Owned(OwnedSection),
}

impl<'a> wasm_encoder::Encode for CowSection<'a> {
    fn encode(&self, sink: &mut Vec<u8>) {
        match self {
            CowSection::Borrowed(b) => b.encode(sink),
            CowSection::Owned(o) => o.encode(sink),
        }
    }
}

impl<'a> wasm_encoder::Section for CowSection<'a> {
    fn id(&self) -> u8 {
        match self {
            CowSection::Borrowed(b) => b.id(),
            CowSection::Owned(o) => o.id(),
        }
    }
}

struct OwnedSection {
    id: u8,
    data: Vec<u8>,
}

impl wasm_encoder::Encode for OwnedSection {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.extend(&self.data);
    }
}

impl wasm_encoder::Section for OwnedSection {
    fn id(&self) -> u8 {
        self.id
    }
}

fn borrowed<'a, T>(
    full_wasm: &'a [u8],
    reader: wasmparser::SectionLimited<T>,
    id: SectionId,
) -> CowSection<'a> {
    CowSection::Borrowed(wasm_encoder::RawSection {
        id: id as _,
        data: &full_wasm[reader.range()],
    })
}

fn owned<'a>(section: impl wasm_encoder::Section) -> CowSection<'a> {
    let mut data = vec![];
    section.encode(&mut data);
    CowSection::Owned(OwnedSection {
        id: section.id(),
        data,
    })
}

/// The instrumentation strategy for recording profiling data.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InstrumentationStrategy {
    /// A low-overhead but imprecise instrumentation strategy.
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
    /// ;; $last_callee += 1
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
    /// profiles using the [`ProfileBuilder`] type.
    ///
    /// Note that, while this strategy yields precise profiling information, it
    /// incurs fairly high overheads, likely making it unacceptable to run the
    /// instrumented Wasm programs in production scenarios.
    PreciseHostCalls,
}

impl FromStr for InstrumentationStrategy {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "three-globals" => Ok(InstrumentationStrategy::ThreeGlobals),
            "precise-host-calls" => Ok(InstrumentationStrategy::PreciseHostCalls),
            _ => anyhow::bail!(
                "Unknown instrumentation strategy '{s}'; valid strategies are: three-globals, \
                 precise-host-calls"
            ),
        }
    }
}
