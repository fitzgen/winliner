//! Optimizing a Wasm program, assuming the behavior observed in the given
//! profile.

use std::collections::{HashMap, HashSet};

use crate::cow_section::{borrowed, owned, CowSection};
use crate::Profile;
use anyhow::{bail, ensure, Result};
use wasm_encoder::SectionId;
use wasmparser::{Chunk, Payload};

#[cfg(feature = "clap")]
use clap::Parser;

/// Optimize a Wasm program based on profiling data.
///
/// # Example
///
/// ```
/// # fn foo() -> anyhow::Result<()> {
/// use winliner::{Optimizer, Profile};
///
/// // Create and configure an optimizer.
/// let mut optimizer = Optimizer::new();
/// optimizer
///     .min_total_calls(100)
///     .min_ratio(0.99)?
///     .max_inline_depth(5);
///
/// // Get the original, uninstrumented Wasm program.
/// let wasm = std::fs::read("path/to/my.wasm")?;
///
/// // Get a profile for our Wasm program from somewhere. Read it from disk,
/// // record it now in this process, etc...
/// let profile = Profile::default();
///
/// // Run the optimizer with the given profile!
/// let optimized_wasm = optimizer.optimize(&profile, &wasm)?;
/// # Ok(()) }
/// ```
#[cfg_attr(feature = "clap", derive(Parser))]
pub struct Optimizer {
    /// The minimum number of total calls for a call site before it is
    /// considered for winlining.
    #[cfg_attr(feature = "clap", clap(long, default_value = "1000"))]
    min_total_calls: u64,

    /// The minimum ratio of all calls at a call site that go to a particular
    /// callee before the callee is considered for winlining. Must be between
    /// 0.0 and 1.0.
    #[cfg_attr(feature = "clap", clap(long, default_value = "0.9"))]
    min_ratio: f64,

    /// The maximum inlining depth.
    ///
    /// This can help limit code size blowup from duplicating many function
    /// bodies during inlining.
    #[cfg_attr(feature = "clap", clap(long, default_value = "1"))]
    max_inline_depth: usize,

    /// Emit feedback counters for how often our speculative inlining guesses
    /// were correct or incorrect.
    ///
    /// When this option is enabled, we will add two globals for each call site
    /// where we winlined a speculative callee:
    ///
    /// 1. A counter for how many times we guessed correctly.
    ///
    /// 2. A counter for how many times we guessed incorrectly.
    ///
    /// You can extract this data when using Winliner as a library with the
    /// `FeedbackCounters` type.
    #[cfg_attr(feature = "clap", clap(long))]
    emit_feedback_counters: bool,

    /// Set the maximum optimization fuel.
    ///
    /// This option is useful for Winliner developers, and unlikely to be useful
    /// to anyone else.
    ///
    /// Allows bisecting optimization bugs to find which optimization site is
    /// buggy.
    #[cfg_attr(feature = "clap", clap(long))]
    fuel: Option<u32>,
}

impl Default for Optimizer {
    fn default() -> Self {
        Optimizer {
            min_total_calls: 1000,
            min_ratio: 0.9,
            max_inline_depth: 1,
            emit_feedback_counters: false,
            fuel: None,
        }
    }
}

impl Optimizer {
    /// Create a new, default optimizer.
    pub fn new() -> Self {
        Default::default()
    }

    /// The minimum number of total calls for a call site before it is
    /// considered for winlining.
    pub fn min_total_calls(&mut self, min: u64) -> &mut Self {
        self.min_total_calls = min;
        self
    }

    /// The minimum ratio of all calls at a call site that go to a particular
    /// callee before the callee is considered for winlining.
    ///
    /// Must be between 0.0 and 1.0.
    pub fn min_ratio(&mut self, min: f64) -> Result<&mut Self> {
        ensure!(
            0.0 <= min && min <= 1.0,
            "The `min_ratio` value must be between 0.0 and 1.0",
        );
        self.min_ratio = min;
        Ok(self)
    }

    /// The maximum inlining depth.
    ///
    /// This can help limit code size blowup from duplicating many function
    /// bodies during inlining.
    pub fn max_inline_depth(&mut self, max: usize) -> &mut Self {
        self.max_inline_depth = max;
        self
    }

    /// Whether to emit feedback counters for how often our speculative inlining
    /// guesses were correct or incorrect.
    ///
    /// When this option is enabled, we will add two globals for each call site
    /// where we winlined a speculative callee:
    ///
    /// 1. A counter for how many times we guessed correctly.
    ///
    /// 2. A counter for how many times we guessed incorrectly.
    ///
    /// You can extract this data using the
    /// [`FeedbackCounters`][crate::FeedbackCounters] type.
    ///
    /// This option is `false` by default.
    pub fn emit_feedback_counters(&mut self, emit: bool) -> &mut Self {
        self.emit_feedback_counters = emit;
        self
    }

    /// Set the maximum optimization fuel.
    ///
    /// This option is useful for Winliner developers, and unlikely to be useful
    /// to anyone else.
    ///
    /// Allows bisecting optimization bugs to find which optimization site is
    /// buggy.
    pub fn fuel(&mut self, fuel: Option<u32>) -> &mut Self {
        self.fuel = fuel;
        self
    }

    /// Optimize the given Wasm binary.
    ///
    /// Callers must ensure that:
    ///
    /// 1. The given Wasm must be the original, uninstrumented Wasm program.
    ///
    /// 2. The profile must have been created from an instrumented version of
    ///    this Wasm program.
    ///
    /// Failure to satisfy these requirements may result in a mis-optimized Wasm
    /// binary that has divergent behavior from the original Wasm program.
    pub fn optimize(&self, profile: &Profile, wasm: &[u8]) -> Result<Vec<u8>> {
        // NB: Have to re-validate because the `clap`-parsed values aren't
        // validated upon construction.
        if self.min_ratio < 0.0 || 1.0 < self.min_ratio {
            bail!("The `--min-ratio` value must be between 0.0 and 1.0");
        }

        let mut wasm = wasm;
        let mut parser = wasmparser::Parser::new(0);

        let mut context = OptimizeContext {
            fuel: self.fuel,
            id_counter: 0,
            full_wasm: wasm,
            profile,
            num_imported_funcs: 0,
            types: vec![],
            funcs: vec![],
            first_call_site_offset_for_func: vec![],
            tables: TablesInfo::default(),
            func_bodies: vec![],
            new_global_section: if self.emit_feedback_counters {
                Some(wasm_encoder::GlobalSection::new())
            } else {
                None
            },
            new_export_section: if self.emit_feedback_counters {
                Some(wasm_encoder::ExportSection::new())
            } else {
                None
            },
        };

        // The list of `wasm_encoder` sections we will join together as the new,
        // instrumented Wasm binary.
        let mut new_sections: Vec<CowSection> = vec![];

        loop {
            let eof = true;
            let (consumed, payload) = match parser.parse(wasm, eof)? {
                Chunk::NeedMoreData(_) => unreachable!(),
                Chunk::Parsed { consumed, payload } => (consumed, payload),
            };

            match payload {
                Payload::Version { .. } => {}

                Payload::CustomSection(custom) => {
                    new_sections.push(CowSection::Borrowed(wasm_encoder::RawSection {
                        id: SectionId::Custom as _,
                        data: &context.full_wasm[custom.range()],
                    }))
                }

                Payload::TypeSection(tys) => {
                    borrowed(
                        &mut new_sections,
                        context.full_wasm,
                        tys.clone(),
                        SectionId::Type,
                    );
                    for rec_group in tys.into_iter() {
                        for ty in rec_group?.types() {
                            context.types.push(ty.clone());
                        }
                    }
                }

                Payload::ImportSection(imports) => {
                    borrowed(
                        &mut new_sections,
                        context.full_wasm,
                        imports.clone(),
                        SectionId::Import,
                    );
                    for imp in imports.into_iter() {
                        let imp = imp?;
                        match imp.ty {
                            wasmparser::TypeRef::Func(_) => context.num_imported_funcs += 1,
                            wasmparser::TypeRef::Table(table_ty) => {
                                context.tables.push_imported_table(&table_ty)
                            }
                            _ => {}
                        }
                    }
                }

                Payload::FunctionSection(funcs) => {
                    borrowed(
                        &mut new_sections,
                        context.full_wasm,
                        funcs.clone(),
                        SectionId::Function,
                    );
                    for func_ty in funcs.into_iter() {
                        context.funcs.push(func_ty?);
                    }
                }

                Payload::TableSection(ts) => {
                    borrowed(
                        &mut new_sections,
                        context.full_wasm,
                        ts.clone(),
                        SectionId::Table,
                    );
                    for table in ts.into_iter() {
                        let table = table?;
                        context.tables.push_defined_table(&table);
                    }
                }

                Payload::MemorySection(memories) => borrowed(
                    &mut new_sections,
                    context.full_wasm,
                    memories,
                    SectionId::Memory,
                ),

                Payload::TagSection(tags) => {
                    borrowed(&mut new_sections, context.full_wasm, tags, SectionId::Tag)
                }

                Payload::GlobalSection(globals) => {
                    if self.emit_feedback_counters {
                        let new_global_section = context.new_global_section.as_mut().unwrap();
                        for global in globals.into_iter() {
                            let global = global?;
                            new_global_section
                                .global(global.ty.into(), &global.init_expr.try_into()?);
                        }
                    } else {
                        borrowed(
                            &mut new_sections,
                            context.full_wasm,
                            globals,
                            SectionId::Global,
                        )
                    }
                }

                Payload::ExportSection(exports) => {
                    if self.emit_feedback_counters {
                        let new_export_section = context.new_export_section.as_mut().unwrap();
                        for export in exports.into_iter() {
                            let export = export?;
                            new_export_section.export(
                                export.name,
                                export.kind.into(),
                                export.index,
                            );
                        }
                    } else {
                        borrowed(
                            &mut new_sections,
                            context.full_wasm,
                            exports,
                            SectionId::Export,
                        )
                    }
                }

                Payload::StartSection { func, range: _ } => owned(
                    &mut new_sections,
                    wasm_encoder::StartSection {
                        function_index: func,
                    },
                ),

                Payload::ElementSection(elements) => {
                    borrowed(
                        &mut new_sections,
                        context.full_wasm,
                        elements.clone(),
                        SectionId::Element,
                    );
                    for elem in elements.into_iter() {
                        let elem = elem?;
                        match elem.kind {
                            wasmparser::ElementKind::Active {
                                table_index,
                                offset_expr,
                            } => {
                                let mut ops = offset_expr.get_operators_reader().into_iter();
                                match (ops.next(), ops.next(), ops.next()) {
                                    (
                                        Some(Ok(wasmparser::Operator::I32Const { value })),
                                        Some(Ok(wasmparser::Operator::End)),
                                        None,
                                    ) => {
                                        context.tables[table_index.unwrap_or(0)]
                                            .add_elements(value as u32, elem.items)?;
                                    }
                                    _ => log::warn!("Ignoring non-constant element segment"),
                                }
                            }
                            wasmparser::ElementKind::Passive
                            | wasmparser::ElementKind::Declared => {}
                        }
                    }
                }

                Payload::DataCountSection { count, range: _ } => {
                    owned(&mut new_sections, wasm_encoder::DataCountSection { count })
                }

                Payload::CodeSectionStart { .. } => {}
                Payload::CodeSectionEntry(body) => context.func_bodies.push(body),

                Payload::DataSection(data) => {
                    borrowed(&mut new_sections, context.full_wasm, data, SectionId::Data)
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

            wasm = &wasm[consumed..];
        }

        let mut new_code_section = Some(self.optimize_func_bodies(&mut context)?);

        log::trace!("Building final optimized module");
        let mut module = wasm_encoder::Module::new();
        for section in &new_sections {
            use wasm_encoder::Section;

            if context
                .new_global_section
                .as_ref()
                .map_or(false, |s| s.id() < section.id())
            {
                let s = context.new_global_section.take().unwrap();
                log::trace!("Appending section id: {}", s.id());
                module.section(&s);
            }

            if context
                .new_export_section
                .as_ref()
                .map_or(false, |s| s.id() < section.id())
            {
                let s = context.new_export_section.take().unwrap();
                log::trace!("Appending section id: {}", s.id());
                module.section(&s);
            }

            if new_code_section.as_ref().map_or(false, |s| {
                s.id() < section.id() && section.id() != SectionId::DataCount as u8
            }) {
                let s = new_code_section.take().unwrap();
                log::trace!("Appending section id: {}", s.id());
                module.section(&s);
            }

            log::trace!("Appending section id: {}", section.id());
            module.section(&*section);
        }
        if let Some(s) = new_code_section {
            module.section(&s);
        }
        Ok(module.finish())
    }

    fn optimize_func_bodies(
        &self,
        context: &mut OptimizeContext,
    ) -> Result<wasm_encoder::CodeSection> {
        let mut call_site_index = 0;
        for body in context.func_bodies.iter() {
            context
                .first_call_site_offset_for_func
                .push(call_site_index);
            for op in body.get_operators_reader()?.into_iter() {
                match op? {
                    wasmparser::Operator::CallIndirect { .. } => call_site_index += 1,
                    _ => {}
                }
            }
        }

        let mut new_code_section = wasm_encoder::CodeSection::new();
        for (defined_func_index, body) in context.func_bodies.clone().into_iter().enumerate() {
            let func_type = context.funcs[defined_func_index];
            let defined_func_index = u32::try_from(defined_func_index).unwrap();
            let func = self.optimize_one_func_body(context, defined_func_index, func_type, body)?;
            new_code_section.function(&func);
        }
        Ok(new_code_section)
    }

    fn optimize_one_func_body<'a>(
        &self,
        context: &mut OptimizeContext<'a, '_>,
        defined_func_index: u32,
        func_type: u32,
        func_body: wasmparser::FunctionBody<'a>,
    ) -> Result<wasm_encoder::Function> {
        // Copy the locals to the new function.
        let mut locals = vec![];
        let func_type = &context.types[usize::try_from(func_type).unwrap()];
        let mut num_locals = match &func_type.structural_type {
            wasmparser::StructuralType::Func(ty) => u32::try_from(ty.params().len()).unwrap(),
            _ => bail!("invalid type for defined function {defined_func_index}"),
        };
        for local in func_body.get_locals_reader()?.into_iter() {
            let (count, ty) = local?;
            num_locals += count;
            locals.push((count, ty.into()));
        }

        // Add our temporary local where we save a copy of indirect callee
        // indices.
        let temp_callee_local = num_locals;
        locals.push((1, wasm_encoder::ValType::I32));
        num_locals += 1;

        // The instructions making up the new body of the optimized function.
        let mut new_insts: Vec<CowInst> = vec![];

        // Stack of functions to copy over to the new, optimized function. The
        // root is the original function itself and any subsequent entries are
        // being inlined into it. As we find a `call_indirect` that we'd like to
        // winline, we push new entries, as we finish winlining we pop
        // enties. Once we've popped the initial entry, we are done rewriting
        // this function.
        let mut stack = vec![{
            let ops = func_body
                .get_operators_reader()?
                .into_iter_with_offsets()
                .peekable();
            StackEntry {
                call_site_index: context.first_call_site_offset_for_func
                    [usize::try_from(defined_func_index).unwrap()],
                defined_func_index,
                locals_delta: 0,
                func_body,
                ops,
                call_indirect_info: None,
                counters: None,
                label_depth_to_return_to: None,
            }
        }];

        // The set of defined function indices that are in the process of being
        // copyied/inlined and are on our stack. We use this to break inlining
        // cycles.
        let mut on_stack: HashSet<u32> = HashSet::from_iter(Some(defined_func_index));

        while let Some(entry) = stack.last_mut() {
            use wasmparser::Operator;

            let (op, offset) = match entry.ops.next() {
                Some(x) => x?,
                None => {
                    // If we did not just finish the outermost function, then we
                    // need to add the fallback path for when we incorrectly
                    // guess the callee.
                    if let Some(info) = entry.call_indirect_info {
                        // This is what we want `new_insts` to look like:
                        //
                        //     ...
                        //     if
                        //       <inlined body>
                        //       end             <---- This is last instruction
                        //                             currently in `new_insts` and
                        //                             everything following needs to
                        //                             be inserted.
                        //     else
                        //       local.get $temp_callee_local
                        //       call_indirect
                        //     end
                        //     ...
                        //
                        // Except that we don't want the `end` that immediately
                        // follows the inlined body. Note that it is there
                        // because Wasm functions always terminate with an `end`
                        // instruction.  We don't want it in our inlined version
                        // of the function because it would terminate the `if`
                        // block without a chance for us to add our `else`. So
                        // pop it now; we can reuse it for closing the `else`.
                        let end = new_insts
                            .pop()
                            .expect("inlined function should have ended with `end` instruction");

                        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::Else));

                        if self.emit_feedback_counters {
                            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::GlobalGet(
                                entry.counters.unwrap().1,
                            )));
                            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::I64Const(1)));
                            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::I64Add));
                            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::GlobalSet(
                                entry.counters.unwrap().1,
                            )));
                        }

                        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::LocalGet(
                            temp_callee_local,
                        )));
                        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::CallIndirect {
                            ty: info.type_index,
                            table: info.table_index,
                        }));
                        new_insts.push(end);
                    }

                    on_stack.remove(&entry.defined_func_index);
                    stack.pop();
                    continue;
                }
            };

            match op {
                Operator::CallIndirect {
                    type_index,
                    table_index,
                    table_byte: _,
                } => {
                    entry.call_site_index += 1;
                    if let Some(new_entry) = self.try_enqueue_for_winlining(
                        context,
                        &on_stack,
                        &mut locals,
                        &mut num_locals,
                        &mut new_insts,
                        temp_callee_local,
                        entry.call_site_index - 1,
                        table_index,
                        type_index,
                    )? {
                        on_stack.insert(new_entry.defined_func_index);
                        stack.push(new_entry);
                    } else {
                        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::CallIndirect {
                            ty: type_index,
                            table: table_index,
                        }));
                    }
                }

                // `local.{get,set,tee}` instruction's need their local index adjusted.
                Operator::LocalGet { local_index } if entry.locals_delta != 0 => {
                    new_insts.push(CowInst::Owned(wasm_encoder::Instruction::LocalGet(
                        local_index + entry.locals_delta,
                    )))
                }
                Operator::LocalSet { local_index } if entry.locals_delta != 0 => {
                    new_insts.push(CowInst::Owned(wasm_encoder::Instruction::LocalSet(
                        local_index + entry.locals_delta,
                    )))
                }
                Operator::LocalTee { local_index } if entry.locals_delta != 0 => {
                    new_insts.push(CowInst::Owned(wasm_encoder::Instruction::LocalTee(
                        local_index + entry.locals_delta,
                    )))
                }

                // Returns inside of inlined callees need to become branches out
                // of the inline body -- not returns from the caller function.
                Operator::Return => {
                    new_insts.push(CowInst::Owned(
                        if let Some(n) = entry.label_depth_to_return_to {
                            wasm_encoder::Instruction::Br(n)
                        } else {
                            wasm_encoder::Instruction::Return
                        },
                    ));
                }

                // TODO(#9): Support `return_call` and `return_call_indirect`
                Operator::ReturnCall { .. } => bail!("`return_call` is not currently supported"),
                Operator::ReturnCallIndirect { .. } => {
                    bail!("`return_call_indirect` is not currently supported")
                }

                // All other instructions can just be copied over!
                op => {
                    // Bookkeeping to update `label_depth_to_return_to` as
                    // necessary when we enter and exit control blocks.
                    if let Some(n) = entry.label_depth_to_return_to.as_mut() {
                        match op {
                            Operator::Block { .. }
                            | Operator::Loop { .. }
                            | Operator::If { .. } => {
                                *n += 1;
                            }
                            Operator::End => {
                                // NB: has to be a saturating decrement because
                                // all function bodies are terminated with an
                                // `end` that didn't have a corresponding
                                // `block` or whatever increment.
                                *n = n.saturating_sub(1);
                            }
                            _ => {}
                        }
                    }

                    let start = offset;

                    // Find the start of the next instruction, aka
                    // the end of this instruction, and copy over
                    // this instruction's raw bytes to the new
                    // function.
                    let end = entry
                        .ops
                        .peek()
                        .map_or(Ok(entry.func_body.range().end), |res| {
                            res.clone().map(|(_, off)| off)
                        })?;

                    new_insts.push(CowInst::Borrowed(&context.full_wasm[start..end]));
                }
            }
        }

        let mut func = wasm_encoder::Function::new(locals);
        for inst in new_insts {
            match inst {
                CowInst::Borrowed(bytes) => {
                    func.raw(bytes.iter().copied());
                }
                CowInst::Owned(inst) => {
                    func.instruction(&inst);
                }
            }
        }
        Ok(func)
    }

    fn try_enqueue_for_winlining<'a, 'b>(
        &self,
        context: &mut OptimizeContext<'a, 'b>,
        on_stack: &HashSet<u32>,
        locals: &mut Vec<(u32, wasm_encoder::ValType)>,
        num_locals: &mut u32,
        new_insts: &mut Vec<CowInst>,
        temp_callee_local: u32,
        call_site_index: u32,
        table_index: u32,
        type_index: u32,
    ) -> Result<Option<StackEntry<'a>>> {
        // Check whether we have enough fuel to perform this winlining, and
        // consume one unit of fuel if we do.
        if let Some(fuel) = context.fuel.as_mut() {
            if *fuel == 0 {
                return Ok(None);
            }
            *fuel -= 1;
        }

        // If we haven't already reached our maximum inlining depth...
        if (on_stack.len() - 1) >= self.max_inline_depth {
            return Ok(None);
        }

        // ... and we have profiling information for this call site...
        let call_site = match context.profile.call_sites.get(&call_site_index) {
            Some(x) => x,
            None => return Ok(None),
        };

        // ... and this call site is hot enough...
        if call_site.total_call_count < self.min_total_calls {
            return Ok(None);
        }

        // ... then get the hottest callee table index...
        let callee = call_site
            .callee_to_count
            .iter()
            .map(|(callee, count)| (*callee, *count))
            .max_by(|a, b| a.1.cmp(&b.1).then(b.0.cmp(&a.0)));
        let (callee_index_in_table, callee_count) = match callee {
            Some(x) => x,
            None => return Ok(None),
        };

        // ... and if that hottest callee is called often enough...
        let callee_ratio = callee_count as f64 / call_site.total_call_count as f64;
        if callee_ratio < self.min_ratio {
            return Ok(None);
        }

        // ... and if we statically know what function index `table[callee]`
        // is...
        let callee_func_index = match context.tables[table_index].get(callee_index_in_table) {
            Some(x) => x,
            None => return Ok(None),
        };

        // ... and if that function index is not already on our winlining stack
        // (i.e. we aren't in a recursive inlining chain)...
        if on_stack.contains(&callee_func_index) {
            return Ok(None);
        }

        // ... and if that function has the correct type (if this is not true
        // then either the profile is bogus/mismatched or every time the call
        // site was executed it trapped)...
        if context.funcs[usize::try_from(callee_func_index - context.num_imported_funcs).unwrap()]
            != type_index
        {
            return Ok(None);
        }

        // ... then we can winline this callee and push it onto our stack!

        let defined_func_index = callee_func_index
            .checked_sub(context.num_imported_funcs)
            .unwrap();
        let locals_delta = *num_locals;
        let func_body = context.func_bodies[usize::try_from(defined_func_index).unwrap()].clone();
        let ops = func_body
            .get_operators_reader()?
            .into_iter_with_offsets()
            .peekable();

        // Add the first half of our speculative inlining sequence, before the
        // inlined body:
        //
        //     local.tee $temp_callee_local
        //     i32.const <callee_index_in_table>
        //     i32.eq
        //     if (param ...) (result ...)
        //       local.set $callee_param_0
        //       local.set $callee_param_1
        //       ...
        //       local.set $callee_param_N   <------- everything here and above
        //       <inlined body>
        //     else
        //       local.get $temp_callee_local
        //       call_indirect
        //     end
        //
        // Note that it might seem like nested inlining might clobber
        // `temp_callee_local`, but it is actually not live in the `if` body,
        // only in the `else` body. Therefore, if nested inlining clobbers the
        // local, it is actually okay, since it won't be read again be this call
        // site. And if it is going to be read again by this call site, then we
        // aren't executing the inline body, and so it can't be clobbered.
        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::LocalTee(
            temp_callee_local,
        )));
        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::I32Const(
            callee_index_in_table as i32,
        )));
        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::I32Eq));
        new_insts.push(CowInst::Owned(wasm_encoder::Instruction::If(
            wasm_encoder::BlockType::FunctionType(type_index),
        )));

        // If we are emitting counters, then create the counters for this
        // winlining and add the increment for the correct-guess counter.
        let counters = if self.emit_feedback_counters {
            let id = context.next_id();
            let new_global_section = context.new_global_section.as_mut().unwrap();
            let new_export_section = context.new_export_section.as_mut().unwrap();

            let correct = new_global_section.len();
            new_global_section.global(
                wasm_encoder::GlobalType {
                    val_type: wasm_encoder::ValType::I64,
                    mutable: true,
                },
                &wasm_encoder::ConstExpr::i64_const(0),
            );
            new_export_section.export(
                &format!("__winliner_counter_{id}_correct"),
                wasm_encoder::ExportKind::Global,
                correct,
            );

            let incorrect = new_global_section.len();
            new_global_section.global(
                wasm_encoder::GlobalType {
                    val_type: wasm_encoder::ValType::I64,
                    mutable: true,
                },
                &wasm_encoder::ConstExpr::i64_const(0),
            );
            new_export_section.export(
                &format!("__winliner_counter_{id}_incorrect"),
                wasm_encoder::ExportKind::Global,
                incorrect,
            );

            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::GlobalGet(
                correct,
            )));
            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::I64Const(1)));
            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::I64Add));
            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::GlobalSet(
                correct,
            )));

            Some((correct, incorrect))
        } else {
            None
        };

        // The callee function assumes that its parameters are in
        // locals, but they are currently on the operand
        // stack. Therefore, we need to "spill" them from the
        // operand stack to a new set of locals in this function and
        // update the locals delta appropriately.
        let ty = match &context.types[usize::try_from(type_index).unwrap()].structural_type {
            wasmparser::StructuralType::Func(ty) => ty,
            _ => bail!("function's type must be a function type"),
        };

        // First, create the locals for the parameters in order.
        for param_ty in ty.params() {
            locals.push((1, (*param_ty).into()));
            *num_locals += 1;
        }

        // Then "spill" them from the operand stack in reverse order.
        for local in (locals_delta..*num_locals).rev() {
            new_insts.push(CowInst::Owned(wasm_encoder::Instruction::LocalSet(local)));
        }

        // Finally, create any additional locals that the callee function needs.
        for l in func_body.get_locals_reader()?.into_iter() {
            let (count, ty) = l?;
            *num_locals += count;
            locals.push((count, ty.into()));
        }

        Ok(Some(StackEntry {
            defined_func_index,
            call_site_index: context.first_call_site_offset_for_func
                [usize::try_from(defined_func_index).unwrap()],
            locals_delta,
            func_body,
            ops,
            call_indirect_info: Some(StackEntryCallIndirectInfo {
                type_index,
                table_index,
            }),
            counters,
            label_depth_to_return_to: Some(0),
        }))
    }
}

/// Common context needed when optimizing function bodies.
struct OptimizeContext<'a, 'b> {
    /// Optimization fuel.
    fuel: Option<u32>,

    /// A counter for generating unique identifiers for each function inlining /
    /// copying.
    ///
    /// Note that this is different from `StackEntry::call_site_index` since the
    /// same call site can be inlined multiple times due to inline chaining.
    id_counter: u32,

    /// The Wasm module's full bytes.
    full_wasm: &'a [u8],

    /// The profile for which we are optimizing with the assumption that future
    /// behavior will match the behavior described in this profile.
    profile: &'b Profile,

    /// The number of imported functions.
    num_imported_funcs: u32,

    /// The types index space.
    types: Vec<wasmparser::SubType>,

    /// A map from defined function index to type.
    funcs: Vec<u32>,

    /// A map from defined function index to the call site index offset for that
    /// function (i.e. the count of how many `call_indirect` instructions
    /// appeared in the code section before this function body).
    first_call_site_offset_for_func: Vec<u32>,

    /// The static information we have about the tables present in the module.
    tables: TablesInfo,

    /// The function bodies for this Wasm module.
    func_bodies: Vec<wasmparser::FunctionBody<'a>>,

    /// The new global section we are building, when we are emitting counters.
    new_global_section: Option<wasm_encoder::GlobalSection>,

    /// The new export section we are building, when we are emitting counters.
    new_export_section: Option<wasm_encoder::ExportSection>,
}

impl OptimizeContext<'_, '_> {
    fn next_id(&mut self) -> u32 {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }
}

/// Information about a function we are currently copying/inlining.
///
/// An entry can either be:
///
/// * The root function which we are copying from the original module to the
///   optimized module. This is always the first entry on the stack.
///
/// * Or a function we are inlining into the next older stack entry's function
///   at a `call_indirect` call site.
struct StackEntry<'a> {
    /// The defined function index of the function we are currently inlining.
    defined_func_index: u32,

    /// The current `call_indirect` index we are processing.
    call_site_index: u32,

    /// The delta to apply to all `local.{get,set,tee}` instructions when
    /// inlining this function body.
    locals_delta: u32,

    /// Information about the `call_indirect` that led to this function getting
    /// inlined. This field is `Some` if and only if this stack entry is a
    /// non-root stack entry.
    call_indirect_info: Option<StackEntryCallIndirectInfo>,

    /// The body of the function we are currently copying/inlining.
    func_body: wasmparser::FunctionBody<'a>,

    /// The iterator of operators within the function we are currently
    /// copying/inlining.
    ops: std::iter::Peekable<wasmparser::OperatorsIteratorWithOffsets<'a>>,

    /// The global indices for the counters for when we guess correctly and
    /// incorrectly, respectively.
    counters: Option<(u32, u32)>,

    /// When an inlined function wants to return, we don't want to actually emit
    /// a `return` instruction since that will exit the root caller. Instead, we
    /// need to translate that to `br N` where `N` is the number of labels in
    /// this callee's inlined body. This `label_depth_to_return_to` field is
    /// that `N`.
    ///
    /// `None` for the root caller.
    label_depth_to_return_to: Option<u32>,
}

/// Information about a `call_indirect` call site for a stack entry.
#[derive(Clone, Copy)]
struct StackEntryCallIndirectInfo {
    type_index: u32,
    table_index: u32,
}

enum CowInst<'a> {
    /// A raw, already-encoded instruction.
    Borrowed(&'a [u8]),

    /// An owned instruction that needs to be encoded.
    Owned(wasm_encoder::Instruction<'a>),
}

/// Static information about the contents of the tables in the Wasm program.
#[derive(Default)]
struct TablesInfo {
    /// Maps from table index to the associated static info about that table's
    /// entries.
    tables: Vec<TableInfo>,
}

impl std::ops::Index<u32> for TablesInfo {
    type Output = TableInfo;

    fn index(&self, index: u32) -> &Self::Output {
        let index = usize::try_from(index).unwrap();
        &self.tables[index]
    }
}

impl std::ops::IndexMut<u32> for TablesInfo {
    fn index_mut(&mut self, index: u32) -> &mut Self::Output {
        let index = usize::try_from(index).unwrap();
        &mut self.tables[index]
    }
}

impl TablesInfo {
    fn push_imported_table(&mut self, _table_type: &wasmparser::TableType) {
        self.tables.push(TableInfo::default());
    }

    fn push_defined_table(&mut self, _table: &wasmparser::Table) {
        self.tables.push(TableInfo::default());
    }
}

/// Static information about the contents of a table in the Wasm program.
#[derive(Default)]
struct TableInfo {
    /// Maps from an index within the table to the function index of the funcref
    /// in that table slot, if any.
    ///
    /// Empty for non-funcref tables.
    ///
    /// Incomplete for imported tables, if they've been configured to be
    /// allowed.
    ///
    /// Incomplete if arbitrary, non-constant element segment offsets are
    /// allowed and present in the Wasm binary.
    entries: HashMap<u32, u32>,
}

impl TableInfo {
    /// Get the function index of the element at `table[tabled_index]`, if that
    /// is statically known.
    fn get(&self, table_index: u32) -> Option<u32> {
        self.entries.get(&table_index).copied()
    }

    /// Record that the given elements live at the given static offset.
    fn add_elements(&mut self, offset: u32, elements: wasmparser::ElementItems) -> Result<()> {
        match elements {
            wasmparser::ElementItems::Functions(funcs) => {
                for (offset, func) in (offset..).zip(funcs.into_iter()) {
                    let func = func?;
                    self.entries.insert(offset, func);
                }
            }
            wasmparser::ElementItems::Expressions(_ref_ty, exprs) => {
                for (offset, expr) in (offset..).zip(exprs.into_iter()) {
                    let expr = expr?;
                    let mut ops = expr.get_operators_reader().into_iter();
                    match (ops.next(), ops.next(), ops.next()) {
                        (
                            Some(Ok(wasmparser::Operator::RefFunc { function_index })),
                            Some(Ok(wasmparser::Operator::End)),
                            None,
                        ) => {
                            self.entries.insert(offset, function_index);
                        }
                        _ => {
                            // Either a null funcref, or this element segment is
                            // for a non-funcref table. Either way, skip it.
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
