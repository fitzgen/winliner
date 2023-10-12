//! Conversions from `wasmparser` types to `wasm-encoder` types.

use anyhow::{bail, Result};

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
            wasm_encoder::ConstExpr::ref_null(hty.into())
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
