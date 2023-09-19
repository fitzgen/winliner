//! Conversions from `wasmparser` types to `wasm-encoder` types.

use anyhow::{bail, Result};

pub fn type_ref(type_ref: wasmparser::TypeRef) -> wasm_encoder::EntityType {
    match type_ref {
        wasmparser::TypeRef::Func(i) => wasm_encoder::EntityType::Function(i),
        wasmparser::TypeRef::Table(t) => wasm_encoder::EntityType::Table(table_type(t)),
        wasmparser::TypeRef::Memory(m) => wasm_encoder::EntityType::Memory(memory_type(m)),
        wasmparser::TypeRef::Global(g) => wasm_encoder::EntityType::Global(global_type(g)),
        wasmparser::TypeRef::Tag(t) => wasm_encoder::EntityType::Tag(tag_type(t)),
    }
}

pub fn table_type(table_ty: wasmparser::TableType) -> wasm_encoder::TableType {
    wasm_encoder::TableType {
        element_type: ref_type(table_ty.element_type),
        minimum: table_ty.initial,
        maximum: table_ty.maximum,
    }
}

pub fn ref_type(ref_type: wasmparser::RefType) -> wasm_encoder::RefType {
    wasm_encoder::RefType {
        nullable: ref_type.is_nullable(),
        heap_type: heap_type(ref_type.heap_type()),
    }
}

pub fn heap_type(heap_type: wasmparser::HeapType) -> wasm_encoder::HeapType {
    match heap_type {
        wasmparser::HeapType::Indexed(i) => wasm_encoder::HeapType::Indexed(i),
        wasmparser::HeapType::Func => wasm_encoder::HeapType::Func,
        wasmparser::HeapType::Extern => wasm_encoder::HeapType::Extern,
        wasmparser::HeapType::Any => wasm_encoder::HeapType::Any,
        wasmparser::HeapType::None => wasm_encoder::HeapType::None,
        wasmparser::HeapType::NoExtern => wasm_encoder::HeapType::NoExtern,
        wasmparser::HeapType::NoFunc => wasm_encoder::HeapType::NoFunc,
        wasmparser::HeapType::Eq => wasm_encoder::HeapType::Eq,
        wasmparser::HeapType::Struct => wasm_encoder::HeapType::Struct,
        wasmparser::HeapType::Array => wasm_encoder::HeapType::Array,
        wasmparser::HeapType::I31 => wasm_encoder::HeapType::I31,
    }
}

pub fn memory_type(memory_ty: wasmparser::MemoryType) -> wasm_encoder::MemoryType {
    wasm_encoder::MemoryType {
        minimum: memory_ty.initial,
        maximum: memory_ty.maximum,
        memory64: memory_ty.memory64,
        shared: memory_ty.shared,
    }
}

pub fn global_type(global_ty: wasmparser::GlobalType) -> wasm_encoder::GlobalType {
    wasm_encoder::GlobalType {
        val_type: val_type(global_ty.content_type),
        mutable: global_ty.mutable,
    }
}

pub fn val_type(val_ty: wasmparser::ValType) -> wasm_encoder::ValType {
    match val_ty {
        wasmparser::ValType::I32 => wasm_encoder::ValType::I32,
        wasmparser::ValType::I64 => wasm_encoder::ValType::I64,
        wasmparser::ValType::F32 => wasm_encoder::ValType::F32,
        wasmparser::ValType::F64 => wasm_encoder::ValType::F64,
        wasmparser::ValType::V128 => wasm_encoder::ValType::V128,
        wasmparser::ValType::Ref(r) => wasm_encoder::ValType::Ref(ref_type(r)),
    }
}

pub fn tag_type(tag_ty: wasmparser::TagType) -> wasm_encoder::TagType {
    wasm_encoder::TagType {
        kind: tag_kind(tag_ty.kind),
        func_type_idx: tag_ty.func_type_idx,
    }
}

pub fn tag_kind(tag_kind: wasmparser::TagKind) -> wasm_encoder::TagKind {
    match tag_kind {
        wasmparser::TagKind::Exception => wasm_encoder::TagKind::Exception,
    }
}

pub fn sub_type(sub_ty: wasmparser::SubType) -> wasm_encoder::SubType {
    wasm_encoder::SubType {
        is_final: sub_ty.is_final,
        supertype_idx: sub_ty.supertype_idx,
        structural_type: structural_type(sub_ty.structural_type),
    }
}

pub fn structural_type(structural_ty: wasmparser::StructuralType) -> wasm_encoder::StructuralType {
    match structural_ty {
        wasmparser::StructuralType::Func(f) => wasm_encoder::StructuralType::Func(func_type(f)),
        wasmparser::StructuralType::Array(a) => wasm_encoder::StructuralType::Array(array_type(a)),
        wasmparser::StructuralType::Struct(s) => {
            wasm_encoder::StructuralType::Struct(struct_type(s))
        }
    }
}

pub fn func_type(func_ty: wasmparser::FuncType) -> wasm_encoder::FuncType {
    wasm_encoder::FuncType::new(
        func_ty.params().iter().cloned().map(val_type),
        func_ty.results().iter().cloned().map(val_type),
    )
}

pub fn array_type(array_ty: wasmparser::ArrayType) -> wasm_encoder::ArrayType {
    wasm_encoder::ArrayType(field_type(array_ty.0))
}

pub fn field_type(field_ty: wasmparser::FieldType) -> wasm_encoder::FieldType {
    wasm_encoder::FieldType {
        element_type: storage_type(field_ty.element_type),
        mutable: field_ty.mutable,
    }
}

pub fn storage_type(storage_ty: wasmparser::StorageType) -> wasm_encoder::StorageType {
    match storage_ty {
        wasmparser::StorageType::I8 => wasm_encoder::StorageType::I8,
        wasmparser::StorageType::I16 => wasm_encoder::StorageType::I16,
        wasmparser::StorageType::Val(v) => wasm_encoder::StorageType::Val(val_type(v)),
    }
}

pub fn struct_type(struct_ty: wasmparser::StructType) -> wasm_encoder::StructType {
    wasm_encoder::StructType {
        fields: struct_ty.fields.iter().cloned().map(field_type).collect(),
    }
}

pub fn external_kind(external_kind: wasmparser::ExternalKind) -> wasm_encoder::ExportKind {
    match external_kind {
        wasmparser::ExternalKind::Func => wasm_encoder::ExportKind::Func,
        wasmparser::ExternalKind::Table => wasm_encoder::ExportKind::Table,
        wasmparser::ExternalKind::Memory => wasm_encoder::ExportKind::Memory,
        wasmparser::ExternalKind::Global => wasm_encoder::ExportKind::Global,
        wasmparser::ExternalKind::Tag => wasm_encoder::ExportKind::Tag,
    }
}

pub fn const_expr(const_expr: wasmparser::ConstExpr) -> Result<wasm_encoder::ConstExpr> {
    const_expr_with_func_delta(const_expr, 0)
}

pub fn const_expr_with_func_delta(
    const_expr: wasmparser::ConstExpr,
    func_delta: u32,
) -> Result<wasm_encoder::ConstExpr> {
    let mut ops = const_expr.get_operators_reader().into_iter();

    let result = match ops.next() {
        Some(Ok(wasmparser::Operator::I32Const { value })) => {
            wasm_encoder::ConstExpr::i32_const(value)
        }
        Some(Ok(wasmparser::Operator::I64Const { value })) => {
            wasm_encoder::ConstExpr::i64_const(value)
        }
        Some(Ok(wasmparser::Operator::F32Const { value })) => {
            wasm_encoder::ConstExpr::f32_const(value.bits() as _)
        }
        Some(Ok(wasmparser::Operator::F64Const { value })) => {
            wasm_encoder::ConstExpr::f64_const(value.bits() as _)
        }
        Some(Ok(wasmparser::Operator::V128Const { value })) => {
            wasm_encoder::ConstExpr::v128_const(i128::from_le_bytes(*value.bytes()))
        }
        Some(Ok(wasmparser::Operator::RefNull { hty })) => {
            wasm_encoder::ConstExpr::ref_null(heap_type(hty))
        }
        Some(Ok(wasmparser::Operator::RefFunc { function_index })) => {
            wasm_encoder::ConstExpr::ref_func(function_index + func_delta)
        }
        Some(Ok(wasmparser::Operator::GlobalGet { global_index })) => {
            wasm_encoder::ConstExpr::global_get(global_index)
        }

        // TODO: support the extended-const proposal.
        Some(Ok(op)) => bail!("invalid const expression: {op:?}"),

        Some(Err(e)) => return Err(e.into()),
        None => bail!("empty const expression is invalid"),
    };

    match (ops.next(), ops.next()) {
        (Some(Ok(wasmparser::Operator::End)), None) => Ok(result),
        _ => bail!("invalid const expression"),
    }
}

// pub fn block_type(block_ty: wasmparser::BlockType) -> wasm_encoder::BlockType {
//     todo!()
// }

// pub fn mem_arg(mem_arg: wasmparser::MemArg) -> wasm_encoder::MemArg {
// }

// pub fn operator(op: wasmparser::Operator) -> Result<wasm_encoder::Instruction<'static>> {
//     Ok(match op {
//         wasmparser::Operator::Unreachable => wasm_encoder::Instruction::Unreachable,
//         wasmparser::Operator::Nop => wasm_encoder::Instruction::Nop,
//         wasmparser::Operator::Block { blockty } => {
//             wasm_encoder::Instruction::Block(block_type(blockty))
//         }
//         wasmparser::Operator::Loop { blockty } => {
//             wasm_encoder::Instruction::Loop(block_type(blockty))
//         }
//         wasmparser::Operator::If { blockty } => wasm_encoder::Instruction::If(block_type(blockty)),
//         wasmparser::Operator::Else => wasm_encoder::Instruction::Else,
//         wasmparser::Operator::Try { blockty } => {
//             wasm_encoder::Instruction::Try(block_type(blockty))
//         }
//         wasmparser::Operator::Catch { tag_index } => wasm_encoder::Instruction::Catch(tag_index),
//         wasmparser::Operator::Throw { tag_index } => wasm_encoder::Instruction::Throw(tag_index),
//         wasmparser::Operator::Rethrow { relative_depth } => {
//             wasm_encoder::Instruction::Rethrow(relative_depth)
//         }
//         wasmparser::Operator::End => wasm_encoder::Instruction::End,
//         wasmparser::Operator::Br { relative_depth } => {
//             wasm_encoder::Instruction::Br(relative_depth)
//         }
//         wasmparser::Operator::BrIf { relative_depth } => {
//             wasm_encoder::Instruction::BrIf(relative_depth)
//         }
//         wasmparser::Operator::BrTable { targets } => {
//             let default = targets.default();
//             wasm_encoder::Instruction::BrTable(
//                 targets.targets().collect::<Result<Vec<_>, _>>()?.into(),
//                 default,
//             )
//         }
//         wasmparser::Operator::Return => wasm_encoder::Instruction::Return,
//         wasmparser::Operator::Call { function_index } => {
//             wasm_encoder::Instruction::Call(function_index)
//         }
//         wasmparser::Operator::CallIndirect {
//             type_index,
//             table_index,
//             table_byte: _,
//         } => wasm_encoder::Instruction::CallIndirect {
//             ty: type_index,
//             table: table_index,
//         },
//         wasmparser::Operator::ReturnCall { function_index } => {
//             wasm_encoder::Instruction::ReturnCall(function_index)
//         }
//         wasmparser::Operator::ReturnCallIndirect {
//             type_index,
//             table_index,
//         } => wasm_encoder::Instruction::ReturnCallIndirect {
//             ty: type_index,
//             table: table_index,
//         },
//         wasmparser::Operator::Delegate { relative_depth } => {
//             wasm_encoder::Instruction::Delegate(relative_depth)
//         }
//         wasmparser::Operator::CatchAll => wasm_encoder::Instruction::CatchAll,
//         wasmparser::Operator::Drop => wasm_encoder::Instruction::Drop,
//         wasmparser::Operator::Select => wasm_encoder::Instruction::Select,
//         wasmparser::Operator::TypedSelect { ty } => {
//             wasm_encoder::Instruction::TypedSelect(val_type(ty))
//         }
//         wasmparser::Operator::LocalGet { local_index } => {
//             wasm_encoder::Instruction::LocalGet(local_index)
//         }
//         wasmparser::Operator::LocalSet { local_index } => {
//             wasm_encoder::Instruction::LocalSet(local_index)
//         }
//         wasmparser::Operator::LocalTee { local_index } => {
//             wasm_encoder::Instruction::LocalTee(local_index)
//         }
//         wasmparser::Operator::GlobalGet { global_index } => {
//             wasm_encoder::Instruction::GlobalGet(global_index)
//         }
//         wasmparser::Operator::GlobalSet { global_index } => {
//             wasm_encoder::Instruction::GlobalSet(global_index)
//         }
//         wasmparser::Operator::I32Load { memarg } => {
//             wasm_encoder::Instruction::I32Load(mem_arg(memarg))
//         }
//         wasmparser::Operator::I64Load { memarg } => {
//             wasm_encoder::Instruction::I64Load(mem_arg(memarg))
//         }
//         wasmparser::Operator::F32Load { memarg } => {
//             wasm_encoder::Instruction::F32Load(mem_arg(memarg))
//         }
//         wasmparser::Operator::F64Load { memarg } => {
//             wasm_encoder::Instruction::F64Load(mem_arg(memarg))
//         }
//         wasmparser::Operator::I32Load8S { memarg } => {
//             wasm_encoder::Instruction::I32Load8S { memarg }
//         }
//         wasmparser::Operator::I32Load8U { memarg } => {
//             wasm_encoder::Instruction::I32Load8U { memarg }
//         }
//         wasmparser::Operator::I32Load16S { memarg } => {
//             wasm_encoder::Instruction::I32Load16S { memarg }
//         }
//         wasmparser::Operator::I32Load16U { memarg } => {
//             wasm_encoder::Instruction::I32Load16U { memarg }
//         }
//         wasmparser::Operator::I64Load8S { memarg } => {
//             wasm_encoder::Instruction::I64Load8S { memarg }
//         }
//         wasmparser::Operator::I64Load8U { memarg } => {
//             wasm_encoder::Instruction::I64Load8U { memarg }
//         }
//         wasmparser::Operator::I64Load16S { memarg } => {
//             wasm_encoder::Instruction::I64Load16S { memarg }
//         }
//         wasmparser::Operator::I64Load16U { memarg } => {
//             wasm_encoder::Instruction::I64Load16U { memarg }
//         }
//         wasmparser::Operator::I64Load32S { memarg } => {
//             wasm_encoder::Instruction::I64Load32S { memarg }
//         }
//         wasmparser::Operator::I64Load32U { memarg } => {
//             wasm_encoder::Instruction::I64Load32U { memarg }
//         }
//         wasmparser::Operator::I32Store { memarg } => wasm_encoder::Instruction::I32Store { memarg },
//         wasmparser::Operator::I64Store { memarg } => wasm_encoder::Instruction::I64Store { memarg },
//         wasmparser::Operator::F32Store { memarg } => wasm_encoder::Instruction::F32Store { memarg },
//         wasmparser::Operator::F64Store { memarg } => wasm_encoder::Instruction::F64Store { memarg },
//         wasmparser::Operator::I32Store8 { memarg } => {
//             wasm_encoder::Instruction::I32Store8 { memarg }
//         }
//         wasmparser::Operator::I32Store16 { memarg } => {
//             wasm_encoder::Instruction::I32Store16 { memarg }
//         }
//         wasmparser::Operator::I64Store8 { memarg } => {
//             wasm_encoder::Instruction::I64Store8 { memarg }
//         }
//         wasmparser::Operator::I64Store16 { memarg } => {
//             wasm_encoder::Instruction::I64Store16 { memarg }
//         }
//         wasmparser::Operator::I64Store32 { memarg } => {
//             wasm_encoder::Instruction::I64Store32 { memarg }
//         }
//         wasmparser::Operator::MemorySize { mem, mem_byte } => {
//             wasm_encoder::Instruction::MemorySize { mem, mem_byte }
//         }
//         wasmparser::Operator::MemoryGrow { mem, mem_byte } => {
//             wasm_encoder::Instruction::MemoryGrow { mem, mem_byte }
//         }
//         wasmparser::Operator::I32Const { value } => wasm_encoder::Instruction::I32Const { value },
//         wasmparser::Operator::I64Const { value } => wasm_encoder::Instruction::I64Const { value },
//         wasmparser::Operator::F32Const { value } => wasm_encoder::Instruction::F32Const { value },
//         wasmparser::Operator::F64Const { value } => wasm_encoder::Instruction::F64Const { value },
//         wasmparser::Operator::RefNull { hty } => wasm_encoder::Instruction::RefNull { hty },
//         wasmparser::Operator::RefIsNull => wasm_encoder::Instruction::RefIsNull,
//         wasmparser::Operator::RefFunc { function_index } => {
//             wasm_encoder::Instruction::RefFunc { function_index }
//         }
//         wasmparser::Operator::I32Eqz => wasm_encoder::Instruction::I32Eqz,
//         wasmparser::Operator::I32Eq => wasm_encoder::Instruction::I32Eq,
//         wasmparser::Operator::I32Ne => wasm_encoder::Instruction::I32Ne,
//         wasmparser::Operator::I32LtS => wasm_encoder::Instruction::I32LtS,
//         wasmparser::Operator::I32LtU => wasm_encoder::Instruction::I32LtU,
//         wasmparser::Operator::I32GtS => wasm_encoder::Instruction::I32GtS,
//         wasmparser::Operator::I32GtU => wasm_encoder::Instruction::I32GtU,
//         wasmparser::Operator::I32LeS => wasm_encoder::Instruction::I32LeS,
//         wasmparser::Operator::I32LeU => wasm_encoder::Instruction::I32LeU,
//         wasmparser::Operator::I32GeS => wasm_encoder::Instruction::I32GeS,
//         wasmparser::Operator::I32GeU => wasm_encoder::Instruction::I32GeU,
//         wasmparser::Operator::I64Eqz => wasm_encoder::Instruction::I64Eqz,
//         wasmparser::Operator::I64Eq => wasm_encoder::Instruction::I64Eq,
//         wasmparser::Operator::I64Ne => wasm_encoder::Instruction::I64Ne,
//         wasmparser::Operator::I64LtS => wasm_encoder::Instruction::I64LtS,
//         wasmparser::Operator::I64LtU => wasm_encoder::Instruction::I64LtU,
//         wasmparser::Operator::I64GtS => wasm_encoder::Instruction::I64GtS,
//         wasmparser::Operator::I64GtU => wasm_encoder::Instruction::I64GtU,
//         wasmparser::Operator::I64LeS => wasm_encoder::Instruction::I64LeS,
//         wasmparser::Operator::I64LeU => wasm_encoder::Instruction::I64LeU,
//         wasmparser::Operator::I64GeS => wasm_encoder::Instruction::I64GeS,
//         wasmparser::Operator::I64GeU => wasm_encoder::Instruction::I64GeU,
//         wasmparser::Operator::F32Eq => wasm_encoder::Instruction::F32Eq,
//         wasmparser::Operator::F32Ne => wasm_encoder::Instruction::F32Ne,
//         wasmparser::Operator::F32Lt => wasm_encoder::Instruction::F32Lt,
//         wasmparser::Operator::F32Gt => wasm_encoder::Instruction::F32Gt,
//         wasmparser::Operator::F32Le => wasm_encoder::Instruction::F32Le,
//         wasmparser::Operator::F32Ge => wasm_encoder::Instruction::F32Ge,
//         wasmparser::Operator::F64Eq => wasm_encoder::Instruction::F64Eq,
//         wasmparser::Operator::F64Ne => wasm_encoder::Instruction::F64Ne,
//         wasmparser::Operator::F64Lt => wasm_encoder::Instruction::F64Lt,
//         wasmparser::Operator::F64Gt => wasm_encoder::Instruction::F64Gt,
//         wasmparser::Operator::F64Le => wasm_encoder::Instruction::F64Le,
//         wasmparser::Operator::F64Ge => wasm_encoder::Instruction::F64Ge,
//         wasmparser::Operator::I32Clz => wasm_encoder::Instruction::I32Clz,
//         wasmparser::Operator::I32Ctz => wasm_encoder::Instruction::I32Ctz,
//         wasmparser::Operator::I32Popcnt => wasm_encoder::Instruction::I32Popcnt,
//         wasmparser::Operator::I32Add => wasm_encoder::Instruction::I32Add,
//         wasmparser::Operator::I32Sub => wasm_encoder::Instruction::I32Sub,
//         wasmparser::Operator::I32Mul => wasm_encoder::Instruction::I32Mul,
//         wasmparser::Operator::I32DivS => wasm_encoder::Instruction::I32DivS,
//         wasmparser::Operator::I32DivU => wasm_encoder::Instruction::I32DivU,
//         wasmparser::Operator::I32RemS => wasm_encoder::Instruction::I32RemS,
//         wasmparser::Operator::I32RemU => wasm_encoder::Instruction::I32RemU,
//         wasmparser::Operator::I32And => wasm_encoder::Instruction::I32And,
//         wasmparser::Operator::I32Or => wasm_encoder::Instruction::I32Or,
//         wasmparser::Operator::I32Xor => wasm_encoder::Instruction::I32Xor,
//         wasmparser::Operator::I32Shl => wasm_encoder::Instruction::I32Shl,
//         wasmparser::Operator::I32ShrS => wasm_encoder::Instruction::I32ShrS,
//         wasmparser::Operator::I32ShrU => wasm_encoder::Instruction::I32ShrU,
//         wasmparser::Operator::I32Rotl => wasm_encoder::Instruction::I32Rotl,
//         wasmparser::Operator::I32Rotr => wasm_encoder::Instruction::I32Rotr,
//         wasmparser::Operator::I64Clz => wasm_encoder::Instruction::I64Clz,
//         wasmparser::Operator::I64Ctz => wasm_encoder::Instruction::I64Ctz,
//         wasmparser::Operator::I64Popcnt => wasm_encoder::Instruction::I64Popcnt,
//         wasmparser::Operator::I64Add => wasm_encoder::Instruction::I64Add,
//         wasmparser::Operator::I64Sub => wasm_encoder::Instruction::I64Sub,
//         wasmparser::Operator::I64Mul => wasm_encoder::Instruction::I64Mul,
//         wasmparser::Operator::I64DivS => wasm_encoder::Instruction::I64DivS,
//         wasmparser::Operator::I64DivU => wasm_encoder::Instruction::I64DivU,
//         wasmparser::Operator::I64RemS => wasm_encoder::Instruction::I64RemS,
//         wasmparser::Operator::I64RemU => wasm_encoder::Instruction::I64RemU,
//         wasmparser::Operator::I64And => wasm_encoder::Instruction::I64And,
//         wasmparser::Operator::I64Or => wasm_encoder::Instruction::I64Or,
//         wasmparser::Operator::I64Xor => wasm_encoder::Instruction::I64Xor,
//         wasmparser::Operator::I64Shl => wasm_encoder::Instruction::I64Shl,
//         wasmparser::Operator::I64ShrS => wasm_encoder::Instruction::I64ShrS,
//         wasmparser::Operator::I64ShrU => wasm_encoder::Instruction::I64ShrU,
//         wasmparser::Operator::I64Rotl => wasm_encoder::Instruction::I64Rotl,
//         wasmparser::Operator::I64Rotr => wasm_encoder::Instruction::I64Rotr,
//         wasmparser::Operator::F32Abs => wasm_encoder::Instruction::F32Abs,
//         wasmparser::Operator::F32Neg => wasm_encoder::Instruction::F32Neg,
//         wasmparser::Operator::F32Ceil => wasm_encoder::Instruction::F32Ceil,
//         wasmparser::Operator::F32Floor => wasm_encoder::Instruction::F32Floor,
//         wasmparser::Operator::F32Trunc => wasm_encoder::Instruction::F32Trunc,
//         wasmparser::Operator::F32Nearest => wasm_encoder::Instruction::F32Nearest,
//         wasmparser::Operator::F32Sqrt => wasm_encoder::Instruction::F32Sqrt,
//         wasmparser::Operator::F32Add => wasm_encoder::Instruction::F32Add,
//         wasmparser::Operator::F32Sub => wasm_encoder::Instruction::F32Sub,
//         wasmparser::Operator::F32Mul => wasm_encoder::Instruction::F32Mul,
//         wasmparser::Operator::F32Div => wasm_encoder::Instruction::F32Div,
//         wasmparser::Operator::F32Min => wasm_encoder::Instruction::F32Min,
//         wasmparser::Operator::F32Max => wasm_encoder::Instruction::F32Max,
//         wasmparser::Operator::F32Copysign => wasm_encoder::Instruction::F32Copysign,
//         wasmparser::Operator::F64Abs => wasm_encoder::Instruction::F64Abs,
//         wasmparser::Operator::F64Neg => wasm_encoder::Instruction::F64Neg,
//         wasmparser::Operator::F64Ceil => wasm_encoder::Instruction::F64Ceil,
//         wasmparser::Operator::F64Floor => wasm_encoder::Instruction::F64Floor,
//         wasmparser::Operator::F64Trunc => wasm_encoder::Instruction::F64Trunc,
//         wasmparser::Operator::F64Nearest => wasm_encoder::Instruction::F64Nearest,
//         wasmparser::Operator::F64Sqrt => wasm_encoder::Instruction::F64Sqrt,
//         wasmparser::Operator::F64Add => wasm_encoder::Instruction::F64Add,
//         wasmparser::Operator::F64Sub => wasm_encoder::Instruction::F64Sub,
//         wasmparser::Operator::F64Mul => wasm_encoder::Instruction::F64Mul,
//         wasmparser::Operator::F64Div => wasm_encoder::Instruction::F64Div,
//         wasmparser::Operator::F64Min => wasm_encoder::Instruction::F64Min,
//         wasmparser::Operator::F64Max => wasm_encoder::Instruction::F64Max,
//         wasmparser::Operator::F64Copysign => wasm_encoder::Instruction::F64Copysign,
//         wasmparser::Operator::I32WrapI64 => wasm_encoder::Instruction::I32WrapI64,
//         wasmparser::Operator::I32TruncF32S => wasm_encoder::Instruction::I32TruncF32S,
//         wasmparser::Operator::I32TruncF32U => wasm_encoder::Instruction::I32TruncF32U,
//         wasmparser::Operator::I32TruncF64S => wasm_encoder::Instruction::I32TruncF64S,
//         wasmparser::Operator::I32TruncF64U => wasm_encoder::Instruction::I32TruncF64U,
//         wasmparser::Operator::I64ExtendI32S => wasm_encoder::Instruction::I64ExtendI32S,
//         wasmparser::Operator::I64ExtendI32U => wasm_encoder::Instruction::I64ExtendI32U,
//         wasmparser::Operator::I64TruncF32S => wasm_encoder::Instruction::I64TruncF32S,
//         wasmparser::Operator::I64TruncF32U => wasm_encoder::Instruction::I64TruncF32U,
//         wasmparser::Operator::I64TruncF64S => wasm_encoder::Instruction::I64TruncF64S,
//         wasmparser::Operator::I64TruncF64U => wasm_encoder::Instruction::I64TruncF64U,
//         wasmparser::Operator::F32ConvertI32S => wasm_encoder::Instruction::F32ConvertI32S,
//         wasmparser::Operator::F32ConvertI32U => wasm_encoder::Instruction::F32ConvertI32U,
//         wasmparser::Operator::F32ConvertI64S => wasm_encoder::Instruction::F32ConvertI64S,
//         wasmparser::Operator::F32ConvertI64U => wasm_encoder::Instruction::F32ConvertI64U,
//         wasmparser::Operator::F32DemoteF64 => wasm_encoder::Instruction::F32DemoteF64,
//         wasmparser::Operator::F64ConvertI32S => wasm_encoder::Instruction::F64ConvertI32S,
//         wasmparser::Operator::F64ConvertI32U => wasm_encoder::Instruction::F64ConvertI32U,
//         wasmparser::Operator::F64ConvertI64S => wasm_encoder::Instruction::F64ConvertI64S,
//         wasmparser::Operator::F64ConvertI64U => wasm_encoder::Instruction::F64ConvertI64U,
//         wasmparser::Operator::F64PromoteF32 => wasm_encoder::Instruction::F64PromoteF32,
//         wasmparser::Operator::I32ReinterpretF32 => wasm_encoder::Instruction::I32ReinterpretF32,
//         wasmparser::Operator::I64ReinterpretF64 => wasm_encoder::Instruction::I64ReinterpretF64,
//         wasmparser::Operator::F32ReinterpretI32 => wasm_encoder::Instruction::F32ReinterpretI32,
//         wasmparser::Operator::F64ReinterpretI64 => wasm_encoder::Instruction::F64ReinterpretI64,
//         wasmparser::Operator::I32Extend8S => wasm_encoder::Instruction::I32Extend8S,
//         wasmparser::Operator::I32Extend16S => wasm_encoder::Instruction::I32Extend16S,
//         wasmparser::Operator::I64Extend8S => wasm_encoder::Instruction::I64Extend8S,
//         wasmparser::Operator::I64Extend16S => wasm_encoder::Instruction::I64Extend16S,
//         wasmparser::Operator::I64Extend32S => wasm_encoder::Instruction::I64Extend32S,
//         wasmparser::Operator::I31New => wasm_encoder::Instruction::I31New,
//         wasmparser::Operator::I31GetS => wasm_encoder::Instruction::I31GetS,
//         wasmparser::Operator::I31GetU => wasm_encoder::Instruction::I31GetU,
//         wasmparser::Operator::I32TruncSatF32S => wasm_encoder::Instruction::I32TruncSatF32S,
//         wasmparser::Operator::I32TruncSatF32U => wasm_encoder::Instruction::I32TruncSatF32U,
//         wasmparser::Operator::I32TruncSatF64S => wasm_encoder::Instruction::I32TruncSatF64S,
//         wasmparser::Operator::I32TruncSatF64U => wasm_encoder::Instruction::I32TruncSatF64U,
//         wasmparser::Operator::I64TruncSatF32S => wasm_encoder::Instruction::I64TruncSatF32S,
//         wasmparser::Operator::I64TruncSatF32U => wasm_encoder::Instruction::I64TruncSatF32U,
//         wasmparser::Operator::I64TruncSatF64S => wasm_encoder::Instruction::I64TruncSatF64S,
//         wasmparser::Operator::I64TruncSatF64U => wasm_encoder::Instruction::I64TruncSatF64U,
//         wasmparser::Operator::MemoryInit { data_index, mem } => {
//             wasm_encoder::Instruction::MemoryInit { data_index, mem }
//         }
//         wasmparser::Operator::DataDrop { data_index } => {
//             wasm_encoder::Instruction::DataDrop { data_index }
//         }
//         wasmparser::Operator::MemoryCopy { dst_mem, src_mem } => {
//             wasm_encoder::Instruction::MemoryCopy { dst_mem, src_mem }
//         }
//         wasmparser::Operator::MemoryFill { mem } => wasm_encoder::Instruction::MemoryFill { mem },
//         wasmparser::Operator::TableInit { elem_index, table } => {
//             wasm_encoder::Instruction::TableInit { elem_index, table }
//         }
//         wasmparser::Operator::ElemDrop { elem_index } => {
//             wasm_encoder::Instruction::ElemDrop { elem_index }
//         }
//         wasmparser::Operator::TableCopy {
//             dst_table,
//             src_table,
//         } => wasm_encoder::Instruction::TableCopy {
//             dst_table,
//             src_table,
//         },
//         wasmparser::Operator::TableFill { table } => wasm_encoder::Instruction::TableFill { table },
//         wasmparser::Operator::TableGet { table } => wasm_encoder::Instruction::TableGet { table },
//         wasmparser::Operator::TableSet { table } => wasm_encoder::Instruction::TableSet { table },
//         wasmparser::Operator::TableGrow { table } => wasm_encoder::Instruction::TableGrow { table },
//         wasmparser::Operator::TableSize { table } => wasm_encoder::Instruction::TableSize { table },
//         wasmparser::Operator::MemoryDiscard { mem } => {
//             wasm_encoder::Instruction::MemoryDiscard { mem }
//         }
//         wasmparser::Operator::MemoryAtomicNotify { memarg } => {
//             wasm_encoder::Instruction::MemoryAtomicNotify { memarg }
//         }
//         wasmparser::Operator::MemoryAtomicWait32 { memarg } => {
//             wasm_encoder::Instruction::MemoryAtomicWait32 { memarg }
//         }
//         wasmparser::Operator::MemoryAtomicWait64 { memarg } => {
//             wasm_encoder::Instruction::MemoryAtomicWait64 { memarg }
//         }
//         wasmparser::Operator::AtomicFence => wasm_encoder::Instruction::AtomicFence,
//         wasmparser::Operator::I32AtomicLoad { memarg } => {
//             wasm_encoder::Instruction::I32AtomicLoad { memarg }
//         }
//         wasmparser::Operator::I64AtomicLoad { memarg } => {
//             wasm_encoder::Instruction::I64AtomicLoad { memarg }
//         }
//         wasmparser::Operator::I32AtomicLoad8U { memarg } => {
//             wasm_encoder::Instruction::I32AtomicLoad8U { memarg }
//         }
//         wasmparser::Operator::I32AtomicLoad16U { memarg } => {
//             wasm_encoder::Instruction::I32AtomicLoad16U { memarg }
//         }
//         wasmparser::Operator::I64AtomicLoad8U { memarg } => {
//             wasm_encoder::Instruction::I64AtomicLoad8U { memarg }
//         }
//         wasmparser::Operator::I64AtomicLoad16U { memarg } => {
//             wasm_encoder::Instruction::I64AtomicLoad16U { memarg }
//         }
//         wasmparser::Operator::I64AtomicLoad32U { memarg } => {
//             wasm_encoder::Instruction::I64AtomicLoad32U { memarg }
//         }
//         wasmparser::Operator::I32AtomicStore { memarg } => {
//             wasm_encoder::Instruction::I32AtomicStore { memarg }
//         }
//         wasmparser::Operator::I64AtomicStore { memarg } => {
//             wasm_encoder::Instruction::I64AtomicStore { memarg }
//         }
//         wasmparser::Operator::I32AtomicStore8 { memarg } => {
//             wasm_encoder::Instruction::I32AtomicStore8 { memarg }
//         }
//         wasmparser::Operator::I32AtomicStore16 { memarg } => {
//             wasm_encoder::Instruction::I32AtomicStore16 { memarg }
//         }
//         wasmparser::Operator::I64AtomicStore8 { memarg } => {
//             wasm_encoder::Instruction::I64AtomicStore8 { memarg }
//         }
//         wasmparser::Operator::I64AtomicStore16 { memarg } => {
//             wasm_encoder::Instruction::I64AtomicStore16 { memarg }
//         }
//         wasmparser::Operator::I64AtomicStore32 { memarg } => {
//             wasm_encoder::Instruction::I64AtomicStore32 { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwAdd { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwAdd { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwAdd { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwAdd { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8AddU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8AddU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16AddU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16AddU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8AddU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8AddU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16AddU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16AddU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32AddU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32AddU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwSub { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwSub { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwSub { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwSub { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8SubU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8SubU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16SubU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16SubU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8SubU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8SubU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16SubU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16SubU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32SubU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32SubU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwAnd { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwAnd { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwAnd { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwAnd { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8AndU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8AndU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16AndU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16AndU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8AndU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8AndU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16AndU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16AndU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32AndU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32AndU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwOr { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwOr { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwOr { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwOr { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8OrU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8OrU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16OrU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16OrU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8OrU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8OrU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16OrU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16OrU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32OrU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32OrU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwXor { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwXor { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwXor { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwXor { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8XorU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8XorU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16XorU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16XorU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8XorU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8XorU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16XorU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16XorU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32XorU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32XorU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwXchg { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwXchg { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwXchg { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwXchg { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8XchgU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8XchgU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16XchgU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16XchgU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8XchgU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8XchgU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16XchgU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16XchgU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32XchgU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32XchgU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmwCmpxchg { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmwCmpxchg { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmwCmpxchg { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmwCmpxchg { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw8CmpxchgU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw8CmpxchgU { memarg }
//         }
//         wasmparser::Operator::I32AtomicRmw16CmpxchgU { memarg } => {
//             wasm_encoder::Instruction::I32AtomicRmw16CmpxchgU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw8CmpxchgU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw8CmpxchgU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw16CmpxchgU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw16CmpxchgU { memarg }
//         }
//         wasmparser::Operator::I64AtomicRmw32CmpxchgU { memarg } => {
//             wasm_encoder::Instruction::I64AtomicRmw32CmpxchgU { memarg }
//         }
//         wasmparser::Operator::V128Load { memarg } => wasm_encoder::Instruction::V128Load { memarg },
//         wasmparser::Operator::V128Load8x8S { memarg } => {
//             wasm_encoder::Instruction::V128Load8x8S { memarg }
//         }
//         wasmparser::Operator::V128Load8x8U { memarg } => {
//             wasm_encoder::Instruction::V128Load8x8U { memarg }
//         }
//         wasmparser::Operator::V128Load16x4S { memarg } => {
//             wasm_encoder::Instruction::V128Load16x4S { memarg }
//         }
//         wasmparser::Operator::V128Load16x4U { memarg } => {
//             wasm_encoder::Instruction::V128Load16x4U { memarg }
//         }
//         wasmparser::Operator::V128Load32x2S { memarg } => {
//             wasm_encoder::Instruction::V128Load32x2S { memarg }
//         }
//         wasmparser::Operator::V128Load32x2U { memarg } => {
//             wasm_encoder::Instruction::V128Load32x2U { memarg }
//         }
//         wasmparser::Operator::V128Load8Splat { memarg } => {
//             wasm_encoder::Instruction::V128Load8Splat { memarg }
//         }
//         wasmparser::Operator::V128Load16Splat { memarg } => {
//             wasm_encoder::Instruction::V128Load16Splat { memarg }
//         }
//         wasmparser::Operator::V128Load32Splat { memarg } => {
//             wasm_encoder::Instruction::V128Load32Splat { memarg }
//         }
//         wasmparser::Operator::V128Load64Splat { memarg } => {
//             wasm_encoder::Instruction::V128Load64Splat { memarg }
//         }
//         wasmparser::Operator::V128Load32Zero { memarg } => {
//             wasm_encoder::Instruction::V128Load32Zero { memarg }
//         }
//         wasmparser::Operator::V128Load64Zero { memarg } => {
//             wasm_encoder::Instruction::V128Load64Zero { memarg }
//         }
//         wasmparser::Operator::V128Store { memarg } => {
//             wasm_encoder::Instruction::V128Store { memarg }
//         }
//         wasmparser::Operator::V128Load8Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Load8Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Load16Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Load16Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Load32Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Load32Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Load64Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Load64Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Store8Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Store8Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Store16Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Store16Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Store32Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Store32Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Store64Lane { memarg, lane } => {
//             wasm_encoder::Instruction::V128Store64Lane { memarg, lane }
//         }
//         wasmparser::Operator::V128Const { value } => wasm_encoder::Instruction::V128Const { value },
//         wasmparser::Operator::I8x16Shuffle { lanes } => {
//             wasm_encoder::Instruction::I8x16Shuffle { lanes }
//         }
//         wasmparser::Operator::I8x16ExtractLaneS { lane } => {
//             wasm_encoder::Instruction::I8x16ExtractLaneS { lane }
//         }
//         wasmparser::Operator::I8x16ExtractLaneU { lane } => {
//             wasm_encoder::Instruction::I8x16ExtractLaneU { lane }
//         }
//         wasmparser::Operator::I8x16ReplaceLane { lane } => {
//             wasm_encoder::Instruction::I8x16ReplaceLane { lane }
//         }
//         wasmparser::Operator::I16x8ExtractLaneS { lane } => {
//             wasm_encoder::Instruction::I16x8ExtractLaneS { lane }
//         }
//         wasmparser::Operator::I16x8ExtractLaneU { lane } => {
//             wasm_encoder::Instruction::I16x8ExtractLaneU { lane }
//         }
//         wasmparser::Operator::I16x8ReplaceLane { lane } => {
//             wasm_encoder::Instruction::I16x8ReplaceLane { lane }
//         }
//         wasmparser::Operator::I32x4ExtractLane { lane } => {
//             wasm_encoder::Instruction::I32x4ExtractLane { lane }
//         }
//         wasmparser::Operator::I32x4ReplaceLane { lane } => {
//             wasm_encoder::Instruction::I32x4ReplaceLane { lane }
//         }
//         wasmparser::Operator::I64x2ExtractLane { lane } => {
//             wasm_encoder::Instruction::I64x2ExtractLane { lane }
//         }
//         wasmparser::Operator::I64x2ReplaceLane { lane } => {
//             wasm_encoder::Instruction::I64x2ReplaceLane { lane }
//         }
//         wasmparser::Operator::F32x4ExtractLane { lane } => {
//             wasm_encoder::Instruction::F32x4ExtractLane { lane }
//         }
//         wasmparser::Operator::F32x4ReplaceLane { lane } => {
//             wasm_encoder::Instruction::F32x4ReplaceLane { lane }
//         }
//         wasmparser::Operator::F64x2ExtractLane { lane } => {
//             wasm_encoder::Instruction::F64x2ExtractLane { lane }
//         }
//         wasmparser::Operator::F64x2ReplaceLane { lane } => {
//             wasm_encoder::Instruction::F64x2ReplaceLane { lane }
//         }
//         wasmparser::Operator::I8x16Swizzle => wasm_encoder::Instruction::I8x16Swizzle,
//         wasmparser::Operator::I8x16Splat => wasm_encoder::Instruction::I8x16Splat,
//         wasmparser::Operator::I16x8Splat => wasm_encoder::Instruction::I16x8Splat,
//         wasmparser::Operator::I32x4Splat => wasm_encoder::Instruction::I32x4Splat,
//         wasmparser::Operator::I64x2Splat => wasm_encoder::Instruction::I64x2Splat,
//         wasmparser::Operator::F32x4Splat => wasm_encoder::Instruction::F32x4Splat,
//         wasmparser::Operator::F64x2Splat => wasm_encoder::Instruction::F64x2Splat,
//         wasmparser::Operator::I8x16Eq => wasm_encoder::Instruction::I8x16Eq,
//         wasmparser::Operator::I8x16Ne => wasm_encoder::Instruction::I8x16Ne,
//         wasmparser::Operator::I8x16LtS => wasm_encoder::Instruction::I8x16LtS,
//         wasmparser::Operator::I8x16LtU => wasm_encoder::Instruction::I8x16LtU,
//         wasmparser::Operator::I8x16GtS => wasm_encoder::Instruction::I8x16GtS,
//         wasmparser::Operator::I8x16GtU => wasm_encoder::Instruction::I8x16GtU,
//         wasmparser::Operator::I8x16LeS => wasm_encoder::Instruction::I8x16LeS,
//         wasmparser::Operator::I8x16LeU => wasm_encoder::Instruction::I8x16LeU,
//         wasmparser::Operator::I8x16GeS => wasm_encoder::Instruction::I8x16GeS,
//         wasmparser::Operator::I8x16GeU => wasm_encoder::Instruction::I8x16GeU,
//         wasmparser::Operator::I16x8Eq => wasm_encoder::Instruction::I16x8Eq,
//         wasmparser::Operator::I16x8Ne => wasm_encoder::Instruction::I16x8Ne,
//         wasmparser::Operator::I16x8LtS => wasm_encoder::Instruction::I16x8LtS,
//         wasmparser::Operator::I16x8LtU => wasm_encoder::Instruction::I16x8LtU,
//         wasmparser::Operator::I16x8GtS => wasm_encoder::Instruction::I16x8GtS,
//         wasmparser::Operator::I16x8GtU => wasm_encoder::Instruction::I16x8GtU,
//         wasmparser::Operator::I16x8LeS => wasm_encoder::Instruction::I16x8LeS,
//         wasmparser::Operator::I16x8LeU => wasm_encoder::Instruction::I16x8LeU,
//         wasmparser::Operator::I16x8GeS => wasm_encoder::Instruction::I16x8GeS,
//         wasmparser::Operator::I16x8GeU => wasm_encoder::Instruction::I16x8GeU,
//         wasmparser::Operator::I32x4Eq => wasm_encoder::Instruction::I32x4Eq,
//         wasmparser::Operator::I32x4Ne => wasm_encoder::Instruction::I32x4Ne,
//         wasmparser::Operator::I32x4LtS => wasm_encoder::Instruction::I32x4LtS,
//         wasmparser::Operator::I32x4LtU => wasm_encoder::Instruction::I32x4LtU,
//         wasmparser::Operator::I32x4GtS => wasm_encoder::Instruction::I32x4GtS,
//         wasmparser::Operator::I32x4GtU => wasm_encoder::Instruction::I32x4GtU,
//         wasmparser::Operator::I32x4LeS => wasm_encoder::Instruction::I32x4LeS,
//         wasmparser::Operator::I32x4LeU => wasm_encoder::Instruction::I32x4LeU,
//         wasmparser::Operator::I32x4GeS => wasm_encoder::Instruction::I32x4GeS,
//         wasmparser::Operator::I32x4GeU => wasm_encoder::Instruction::I32x4GeU,
//         wasmparser::Operator::I64x2Eq => wasm_encoder::Instruction::I64x2Eq,
//         wasmparser::Operator::I64x2Ne => wasm_encoder::Instruction::I64x2Ne,
//         wasmparser::Operator::I64x2LtS => wasm_encoder::Instruction::I64x2LtS,
//         wasmparser::Operator::I64x2GtS => wasm_encoder::Instruction::I64x2GtS,
//         wasmparser::Operator::I64x2LeS => wasm_encoder::Instruction::I64x2LeS,
//         wasmparser::Operator::I64x2GeS => wasm_encoder::Instruction::I64x2GeS,
//         wasmparser::Operator::F32x4Eq => wasm_encoder::Instruction::F32x4Eq,
//         wasmparser::Operator::F32x4Ne => wasm_encoder::Instruction::F32x4Ne,
//         wasmparser::Operator::F32x4Lt => wasm_encoder::Instruction::F32x4Lt,
//         wasmparser::Operator::F32x4Gt => wasm_encoder::Instruction::F32x4Gt,
//         wasmparser::Operator::F32x4Le => wasm_encoder::Instruction::F32x4Le,
//         wasmparser::Operator::F32x4Ge => wasm_encoder::Instruction::F32x4Ge,
//         wasmparser::Operator::F64x2Eq => wasm_encoder::Instruction::F64x2Eq,
//         wasmparser::Operator::F64x2Ne => wasm_encoder::Instruction::F64x2Ne,
//         wasmparser::Operator::F64x2Lt => wasm_encoder::Instruction::F64x2Lt,
//         wasmparser::Operator::F64x2Gt => wasm_encoder::Instruction::F64x2Gt,
//         wasmparser::Operator::F64x2Le => wasm_encoder::Instruction::F64x2Le,
//         wasmparser::Operator::F64x2Ge => wasm_encoder::Instruction::F64x2Ge,
//         wasmparser::Operator::V128Not => wasm_encoder::Instruction::V128Not,
//         wasmparser::Operator::V128And => wasm_encoder::Instruction::V128And,
//         wasmparser::Operator::V128AndNot => wasm_encoder::Instruction::V128AndNot,
//         wasmparser::Operator::V128Or => wasm_encoder::Instruction::V128Or,
//         wasmparser::Operator::V128Xor => wasm_encoder::Instruction::V128Xor,
//         wasmparser::Operator::V128Bitselect => wasm_encoder::Instruction::V128Bitselect,
//         wasmparser::Operator::V128AnyTrue => wasm_encoder::Instruction::V128AnyTrue,
//         wasmparser::Operator::I8x16Abs => wasm_encoder::Instruction::I8x16Abs,
//         wasmparser::Operator::I8x16Neg => wasm_encoder::Instruction::I8x16Neg,
//         wasmparser::Operator::I8x16Popcnt => wasm_encoder::Instruction::I8x16Popcnt,
//         wasmparser::Operator::I8x16AllTrue => wasm_encoder::Instruction::I8x16AllTrue,
//         wasmparser::Operator::I8x16Bitmask => wasm_encoder::Instruction::I8x16Bitmask,
//         wasmparser::Operator::I8x16NarrowI16x8S => wasm_encoder::Instruction::I8x16NarrowI16x8S,
//         wasmparser::Operator::I8x16NarrowI16x8U => wasm_encoder::Instruction::I8x16NarrowI16x8U,
//         wasmparser::Operator::I8x16Shl => wasm_encoder::Instruction::I8x16Shl,
//         wasmparser::Operator::I8x16ShrS => wasm_encoder::Instruction::I8x16ShrS,
//         wasmparser::Operator::I8x16ShrU => wasm_encoder::Instruction::I8x16ShrU,
//         wasmparser::Operator::I8x16Add => wasm_encoder::Instruction::I8x16Add,
//         wasmparser::Operator::I8x16AddSatS => wasm_encoder::Instruction::I8x16AddSatS,
//         wasmparser::Operator::I8x16AddSatU => wasm_encoder::Instruction::I8x16AddSatU,
//         wasmparser::Operator::I8x16Sub => wasm_encoder::Instruction::I8x16Sub,
//         wasmparser::Operator::I8x16SubSatS => wasm_encoder::Instruction::I8x16SubSatS,
//         wasmparser::Operator::I8x16SubSatU => wasm_encoder::Instruction::I8x16SubSatU,
//         wasmparser::Operator::I8x16MinS => wasm_encoder::Instruction::I8x16MinS,
//         wasmparser::Operator::I8x16MinU => wasm_encoder::Instruction::I8x16MinU,
//         wasmparser::Operator::I8x16MaxS => wasm_encoder::Instruction::I8x16MaxS,
//         wasmparser::Operator::I8x16MaxU => wasm_encoder::Instruction::I8x16MaxU,
//         wasmparser::Operator::I8x16AvgrU => wasm_encoder::Instruction::I8x16AvgrU,
//         wasmparser::Operator::I16x8ExtAddPairwiseI8x16S => {
//             wasm_encoder::Instruction::I16x8ExtAddPairwiseI8x16S
//         }
//         wasmparser::Operator::I16x8ExtAddPairwiseI8x16U => {
//             wasm_encoder::Instruction::I16x8ExtAddPairwiseI8x16U
//         }
//         wasmparser::Operator::I16x8Abs => wasm_encoder::Instruction::I16x8Abs,
//         wasmparser::Operator::I16x8Neg => wasm_encoder::Instruction::I16x8Neg,
//         wasmparser::Operator::I16x8Q15MulrSatS => wasm_encoder::Instruction::I16x8Q15MulrSatS,
//         wasmparser::Operator::I16x8AllTrue => wasm_encoder::Instruction::I16x8AllTrue,
//         wasmparser::Operator::I16x8Bitmask => wasm_encoder::Instruction::I16x8Bitmask,
//         wasmparser::Operator::I16x8NarrowI32x4S => wasm_encoder::Instruction::I16x8NarrowI32x4S,
//         wasmparser::Operator::I16x8NarrowI32x4U => wasm_encoder::Instruction::I16x8NarrowI32x4U,
//         wasmparser::Operator::I16x8ExtendLowI8x16S => {
//             wasm_encoder::Instruction::I16x8ExtendLowI8x16S
//         }
//         wasmparser::Operator::I16x8ExtendHighI8x16S => {
//             wasm_encoder::Instruction::I16x8ExtendHighI8x16S
//         }
//         wasmparser::Operator::I16x8ExtendLowI8x16U => {
//             wasm_encoder::Instruction::I16x8ExtendLowI8x16U
//         }
//         wasmparser::Operator::I16x8ExtendHighI8x16U => {
//             wasm_encoder::Instruction::I16x8ExtendHighI8x16U
//         }
//         wasmparser::Operator::I16x8Shl => wasm_encoder::Instruction::I16x8Shl,
//         wasmparser::Operator::I16x8ShrS => wasm_encoder::Instruction::I16x8ShrS,
//         wasmparser::Operator::I16x8ShrU => wasm_encoder::Instruction::I16x8ShrU,
//         wasmparser::Operator::I16x8Add => wasm_encoder::Instruction::I16x8Add,
//         wasmparser::Operator::I16x8AddSatS => wasm_encoder::Instruction::I16x8AddSatS,
//         wasmparser::Operator::I16x8AddSatU => wasm_encoder::Instruction::I16x8AddSatU,
//         wasmparser::Operator::I16x8Sub => wasm_encoder::Instruction::I16x8Sub,
//         wasmparser::Operator::I16x8SubSatS => wasm_encoder::Instruction::I16x8SubSatS,
//         wasmparser::Operator::I16x8SubSatU => wasm_encoder::Instruction::I16x8SubSatU,
//         wasmparser::Operator::I16x8Mul => wasm_encoder::Instruction::I16x8Mul,
//         wasmparser::Operator::I16x8MinS => wasm_encoder::Instruction::I16x8MinS,
//         wasmparser::Operator::I16x8MinU => wasm_encoder::Instruction::I16x8MinU,
//         wasmparser::Operator::I16x8MaxS => wasm_encoder::Instruction::I16x8MaxS,
//         wasmparser::Operator::I16x8MaxU => wasm_encoder::Instruction::I16x8MaxU,
//         wasmparser::Operator::I16x8AvgrU => wasm_encoder::Instruction::I16x8AvgrU,
//         wasmparser::Operator::I16x8ExtMulLowI8x16S => {
//             wasm_encoder::Instruction::I16x8ExtMulLowI8x16S
//         }
//         wasmparser::Operator::I16x8ExtMulHighI8x16S => {
//             wasm_encoder::Instruction::I16x8ExtMulHighI8x16S
//         }
//         wasmparser::Operator::I16x8ExtMulLowI8x16U => {
//             wasm_encoder::Instruction::I16x8ExtMulLowI8x16U
//         }
//         wasmparser::Operator::I16x8ExtMulHighI8x16U => {
//             wasm_encoder::Instruction::I16x8ExtMulHighI8x16U
//         }
//         wasmparser::Operator::I32x4ExtAddPairwiseI16x8S => {
//             wasm_encoder::Instruction::I32x4ExtAddPairwiseI16x8S
//         }
//         wasmparser::Operator::I32x4ExtAddPairwiseI16x8U => {
//             wasm_encoder::Instruction::I32x4ExtAddPairwiseI16x8U
//         }
//         wasmparser::Operator::I32x4Abs => wasm_encoder::Instruction::I32x4Abs,
//         wasmparser::Operator::I32x4Neg => wasm_encoder::Instruction::I32x4Neg,
//         wasmparser::Operator::I32x4AllTrue => wasm_encoder::Instruction::I32x4AllTrue,
//         wasmparser::Operator::I32x4Bitmask => wasm_encoder::Instruction::I32x4Bitmask,
//         wasmparser::Operator::I32x4ExtendLowI16x8S => {
//             wasm_encoder::Instruction::I32x4ExtendLowI16x8S
//         }
//         wasmparser::Operator::I32x4ExtendHighI16x8S => {
//             wasm_encoder::Instruction::I32x4ExtendHighI16x8S
//         }
//         wasmparser::Operator::I32x4ExtendLowI16x8U => {
//             wasm_encoder::Instruction::I32x4ExtendLowI16x8U
//         }
//         wasmparser::Operator::I32x4ExtendHighI16x8U => {
//             wasm_encoder::Instruction::I32x4ExtendHighI16x8U
//         }
//         wasmparser::Operator::I32x4Shl => wasm_encoder::Instruction::I32x4Shl,
//         wasmparser::Operator::I32x4ShrS => wasm_encoder::Instruction::I32x4ShrS,
//         wasmparser::Operator::I32x4ShrU => wasm_encoder::Instruction::I32x4ShrU,
//         wasmparser::Operator::I32x4Add => wasm_encoder::Instruction::I32x4Add,
//         wasmparser::Operator::I32x4Sub => wasm_encoder::Instruction::I32x4Sub,
//         wasmparser::Operator::I32x4Mul => wasm_encoder::Instruction::I32x4Mul,
//         wasmparser::Operator::I32x4MinS => wasm_encoder::Instruction::I32x4MinS,
//         wasmparser::Operator::I32x4MinU => wasm_encoder::Instruction::I32x4MinU,
//         wasmparser::Operator::I32x4MaxS => wasm_encoder::Instruction::I32x4MaxS,
//         wasmparser::Operator::I32x4MaxU => wasm_encoder::Instruction::I32x4MaxU,
//         wasmparser::Operator::I32x4DotI16x8S => wasm_encoder::Instruction::I32x4DotI16x8S,
//         wasmparser::Operator::I32x4ExtMulLowI16x8S => {
//             wasm_encoder::Instruction::I32x4ExtMulLowI16x8S
//         }
//         wasmparser::Operator::I32x4ExtMulHighI16x8S => {
//             wasm_encoder::Instruction::I32x4ExtMulHighI16x8S
//         }
//         wasmparser::Operator::I32x4ExtMulLowI16x8U => {
//             wasm_encoder::Instruction::I32x4ExtMulLowI16x8U
//         }
//         wasmparser::Operator::I32x4ExtMulHighI16x8U => {
//             wasm_encoder::Instruction::I32x4ExtMulHighI16x8U
//         }
//         wasmparser::Operator::I64x2Abs => wasm_encoder::Instruction::I64x2Abs,
//         wasmparser::Operator::I64x2Neg => wasm_encoder::Instruction::I64x2Neg,
//         wasmparser::Operator::I64x2AllTrue => wasm_encoder::Instruction::I64x2AllTrue,
//         wasmparser::Operator::I64x2Bitmask => wasm_encoder::Instruction::I64x2Bitmask,
//         wasmparser::Operator::I64x2ExtendLowI32x4S => {
//             wasm_encoder::Instruction::I64x2ExtendLowI32x4S
//         }
//         wasmparser::Operator::I64x2ExtendHighI32x4S => {
//             wasm_encoder::Instruction::I64x2ExtendHighI32x4S
//         }
//         wasmparser::Operator::I64x2ExtendLowI32x4U => {
//             wasm_encoder::Instruction::I64x2ExtendLowI32x4U
//         }
//         wasmparser::Operator::I64x2ExtendHighI32x4U => {
//             wasm_encoder::Instruction::I64x2ExtendHighI32x4U
//         }
//         wasmparser::Operator::I64x2Shl => wasm_encoder::Instruction::I64x2Shl,
//         wasmparser::Operator::I64x2ShrS => wasm_encoder::Instruction::I64x2ShrS,
//         wasmparser::Operator::I64x2ShrU => wasm_encoder::Instruction::I64x2ShrU,
//         wasmparser::Operator::I64x2Add => wasm_encoder::Instruction::I64x2Add,
//         wasmparser::Operator::I64x2Sub => wasm_encoder::Instruction::I64x2Sub,
//         wasmparser::Operator::I64x2Mul => wasm_encoder::Instruction::I64x2Mul,
//         wasmparser::Operator::I64x2ExtMulLowI32x4S => {
//             wasm_encoder::Instruction::I64x2ExtMulLowI32x4S
//         }
//         wasmparser::Operator::I64x2ExtMulHighI32x4S => {
//             wasm_encoder::Instruction::I64x2ExtMulHighI32x4S
//         }
//         wasmparser::Operator::I64x2ExtMulLowI32x4U => {
//             wasm_encoder::Instruction::I64x2ExtMulLowI32x4U
//         }
//         wasmparser::Operator::I64x2ExtMulHighI32x4U => {
//             wasm_encoder::Instruction::I64x2ExtMulHighI32x4U
//         }
//         wasmparser::Operator::F32x4Ceil => wasm_encoder::Instruction::F32x4Ceil,
//         wasmparser::Operator::F32x4Floor => wasm_encoder::Instruction::F32x4Floor,
//         wasmparser::Operator::F32x4Trunc => wasm_encoder::Instruction::F32x4Trunc,
//         wasmparser::Operator::F32x4Nearest => wasm_encoder::Instruction::F32x4Nearest,
//         wasmparser::Operator::F32x4Abs => wasm_encoder::Instruction::F32x4Abs,
//         wasmparser::Operator::F32x4Neg => wasm_encoder::Instruction::F32x4Neg,
//         wasmparser::Operator::F32x4Sqrt => wasm_encoder::Instruction::F32x4Sqrt,
//         wasmparser::Operator::F32x4Add => wasm_encoder::Instruction::F32x4Add,
//         wasmparser::Operator::F32x4Sub => wasm_encoder::Instruction::F32x4Sub,
//         wasmparser::Operator::F32x4Mul => wasm_encoder::Instruction::F32x4Mul,
//         wasmparser::Operator::F32x4Div => wasm_encoder::Instruction::F32x4Div,
//         wasmparser::Operator::F32x4Min => wasm_encoder::Instruction::F32x4Min,
//         wasmparser::Operator::F32x4Max => wasm_encoder::Instruction::F32x4Max,
//         wasmparser::Operator::F32x4PMin => wasm_encoder::Instruction::F32x4PMin,
//         wasmparser::Operator::F32x4PMax => wasm_encoder::Instruction::F32x4PMax,
//         wasmparser::Operator::F64x2Ceil => wasm_encoder::Instruction::F64x2Ceil,
//         wasmparser::Operator::F64x2Floor => wasm_encoder::Instruction::F64x2Floor,
//         wasmparser::Operator::F64x2Trunc => wasm_encoder::Instruction::F64x2Trunc,
//         wasmparser::Operator::F64x2Nearest => wasm_encoder::Instruction::F64x2Nearest,
//         wasmparser::Operator::F64x2Abs => wasm_encoder::Instruction::F64x2Abs,
//         wasmparser::Operator::F64x2Neg => wasm_encoder::Instruction::F64x2Neg,
//         wasmparser::Operator::F64x2Sqrt => wasm_encoder::Instruction::F64x2Sqrt,
//         wasmparser::Operator::F64x2Add => wasm_encoder::Instruction::F64x2Add,
//         wasmparser::Operator::F64x2Sub => wasm_encoder::Instruction::F64x2Sub,
//         wasmparser::Operator::F64x2Mul => wasm_encoder::Instruction::F64x2Mul,
//         wasmparser::Operator::F64x2Div => wasm_encoder::Instruction::F64x2Div,
//         wasmparser::Operator::F64x2Min => wasm_encoder::Instruction::F64x2Min,
//         wasmparser::Operator::F64x2Max => wasm_encoder::Instruction::F64x2Max,
//         wasmparser::Operator::F64x2PMin => wasm_encoder::Instruction::F64x2PMin,
//         wasmparser::Operator::F64x2PMax => wasm_encoder::Instruction::F64x2PMax,
//         wasmparser::Operator::I32x4TruncSatF32x4S => wasm_encoder::Instruction::I32x4TruncSatF32x4S,
//         wasmparser::Operator::I32x4TruncSatF32x4U => wasm_encoder::Instruction::I32x4TruncSatF32x4U,
//         wasmparser::Operator::F32x4ConvertI32x4S => wasm_encoder::Instruction::F32x4ConvertI32x4S,
//         wasmparser::Operator::F32x4ConvertI32x4U => wasm_encoder::Instruction::F32x4ConvertI32x4U,
//         wasmparser::Operator::I32x4TruncSatF64x2SZero => {
//             wasm_encoder::Instruction::I32x4TruncSatF64x2SZero
//         }
//         wasmparser::Operator::I32x4TruncSatF64x2UZero => {
//             wasm_encoder::Instruction::I32x4TruncSatF64x2UZero
//         }
//         wasmparser::Operator::F64x2ConvertLowI32x4S => {
//             wasm_encoder::Instruction::F64x2ConvertLowI32x4S
//         }
//         wasmparser::Operator::F64x2ConvertLowI32x4U => {
//             wasm_encoder::Instruction::F64x2ConvertLowI32x4U
//         }
//         wasmparser::Operator::F32x4DemoteF64x2Zero => {
//             wasm_encoder::Instruction::F32x4DemoteF64x2Zero
//         }
//         wasmparser::Operator::F64x2PromoteLowF32x4 => {
//             wasm_encoder::Instruction::F64x2PromoteLowF32x4
//         }
//         wasmparser::Operator::I8x16RelaxedSwizzle => wasm_encoder::Instruction::I8x16RelaxedSwizzle,
//         wasmparser::Operator::I32x4RelaxedTruncF32x4S => {
//             wasm_encoder::Instruction::I32x4RelaxedTruncF32x4S
//         }
//         wasmparser::Operator::I32x4RelaxedTruncF32x4U => {
//             wasm_encoder::Instruction::I32x4RelaxedTruncF32x4U
//         }
//         wasmparser::Operator::I32x4RelaxedTruncF64x2SZero => {
//             wasm_encoder::Instruction::I32x4RelaxedTruncF64x2SZero
//         }
//         wasmparser::Operator::I32x4RelaxedTruncF64x2UZero => {
//             wasm_encoder::Instruction::I32x4RelaxedTruncF64x2UZero
//         }
//         wasmparser::Operator::F32x4RelaxedMadd => wasm_encoder::Instruction::F32x4RelaxedMadd,
//         wasmparser::Operator::F32x4RelaxedNmadd => wasm_encoder::Instruction::F32x4RelaxedNmadd,
//         wasmparser::Operator::F64x2RelaxedMadd => wasm_encoder::Instruction::F64x2RelaxedMadd,
//         wasmparser::Operator::F64x2RelaxedNmadd => wasm_encoder::Instruction::F64x2RelaxedNmadd,
//         wasmparser::Operator::I8x16RelaxedLaneselect => {
//             wasm_encoder::Instruction::I8x16RelaxedLaneselect
//         }
//         wasmparser::Operator::I16x8RelaxedLaneselect => {
//             wasm_encoder::Instruction::I16x8RelaxedLaneselect
//         }
//         wasmparser::Operator::I32x4RelaxedLaneselect => {
//             wasm_encoder::Instruction::I32x4RelaxedLaneselect
//         }
//         wasmparser::Operator::I64x2RelaxedLaneselect => {
//             wasm_encoder::Instruction::I64x2RelaxedLaneselect
//         }
//         wasmparser::Operator::F32x4RelaxedMin => wasm_encoder::Instruction::F32x4RelaxedMin,
//         wasmparser::Operator::F32x4RelaxedMax => wasm_encoder::Instruction::F32x4RelaxedMax,
//         wasmparser::Operator::F64x2RelaxedMin => wasm_encoder::Instruction::F64x2RelaxedMin,
//         wasmparser::Operator::F64x2RelaxedMax => wasm_encoder::Instruction::F64x2RelaxedMax,
//         wasmparser::Operator::I16x8RelaxedQ15mulrS => {
//             wasm_encoder::Instruction::I16x8RelaxedQ15mulrS
//         }
//         wasmparser::Operator::I16x8RelaxedDotI8x16I7x16S => {
//             wasm_encoder::Instruction::I16x8RelaxedDotI8x16I7x16S
//         }
//         wasmparser::Operator::I32x4RelaxedDotI8x16I7x16AddS => {
//             wasm_encoder::Instruction::I32x4RelaxedDotI8x16I7x16AddS
//         }
//         wasmparser::Operator::CallRef { type_index } => {
//             wasm_encoder::Instruction::CallRef { type_index }
//         }
//         wasmparser::Operator::ReturnCallRef { type_index } => {
//             wasm_encoder::Instruction::ReturnCallRef { type_index }
//         }
//         wasmparser::Operator::RefAsNonNull => wasm_encoder::Instruction::RefAsNonNull,
//         wasmparser::Operator::BrOnNull { relative_depth } => {
//             wasm_encoder::Instruction::BrOnNull { relative_depth }
//         }
//         wasmparser::Operator::BrOnNonNull { relative_depth } => {
//             wasm_encoder::Instruction::BrOnNonNull { relative_depth }
//         }
//     })
// }
