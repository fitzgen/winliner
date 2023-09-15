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

    super::assert_no_diff(expected_wat.trim(), actual_wat.trim());

    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        function_references: true,
        ..Default::default()
    })
    .validate_all(&actual_wasm)
    .context("the optimized wasm failed to validate")?;

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

#[test]
fn multiple_call_sites() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(2),
        &[&[(0, 2)], &[(1, 1)], &[(2, 2)], &[(3, 1)]],
        r#"
(module
  (type (func (result i32)))

  (func (type 0)
    i32.const 11
  )
  (func (type 0)
    i32.const 22
  )
  (func (type 0)
    i32.const 33
  )
  (func (type 0)
    i32.const 44
  )

  (func (param i32 i32 i32 i32)
    local.get 0
    call_indirect (type 0)
    drop
    local.get 1
    call_indirect (type 0)
    drop
    local.get 2
    call_indirect (type 0)
    drop
    local.get 3
    call_indirect (type 0)
    drop
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1) (ref.func 2) (ref.func 3))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32 i32 i32 i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 0) (result i32)
    (local i32)
    i32.const 22
  )
  (func (;2;) (type 0) (result i32)
    (local i32)
    i32.const 33
  )
  (func (;3;) (type 0) (result i32)
    (local i32)
    i32.const 44
  )
  (func (;4;) (type 1) (param i32 i32 i32 i32)
    (local i32)
    local.get 0
    local.tee 4
    i32.const 0
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 11
    else
      local.get 4
      call_indirect (type 0)
    end
    drop
    local.get 1
    call_indirect (type 0)
    drop
    local.get 2
    local.tee 4
    i32.const 2
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 33
    else
      local.get 4
      call_indirect (type 0)
    end
    drop
    local.get 3
    call_indirect (type 0)
    drop
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1) (ref.func 2) (ref.func 3))
)
        "#,
    )
}

#[test]
fn no_indirect_calls() -> Result<()> {
    assert_optimize(
        &Optimizer::new(),
        &[],
        r#"
(module
  (type (func (result i32)))

  (func (type 0)
    i32.const 11
  )
  (func (type 0)
    i32.const 22
  )
  (func (type 0)
    i32.const 33
  )
  (func (type 0)
    i32.const 44
  )

  (func
    call 0
    drop
    call 1
    drop
    call 2
    drop
    call 3
    drop
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1) (ref.func 2) (ref.func 3))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 0) (result i32)
    (local i32)
    i32.const 22
  )
  (func (;2;) (type 0) (result i32)
    (local i32)
    i32.const 33
  )
  (func (;3;) (type 0) (result i32)
    (local i32)
    i32.const 44
  )
  (func (;4;) (type 1)
    (local i32)
    call 0
    drop
    call 1
    drop
    call 2
    drop
    call 3
    drop
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1) (ref.func 2) (ref.func 3))
)
        "#,
    )
}

// TODO FITZGEN: multiple params and multiple returns
#[test]
fn multiple_params_and_results() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1),
        &[&[(42, 1)]],
        r#"
(module
  (type (func (param i32 i64) (result i32 i64)))

  (func (type 0)
    local.get 0
    i32.const 1
    i32.add
    local.get 1
    i64.const 1
    i64.add
  )

  (func (param i32) (result i32 i64)
    i32.const 11
    i64.const 22
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 42) funcref (ref.func 0))
)
        "#,
        r#"
(module
  (type (;0;) (func (param i32 i64) (result i32 i64)))
  (type (;1;) (func (param i32) (result i32 i64)))
  (func (;0;) (type 0) (param i32 i64) (result i32 i64)
    (local i32)
    local.get 0
    i32.const 1
    i32.add
    local.get 1
    i64.const 1
    i64.add
  )
  (func (;1;) (type 1) (param i32) (result i32 i64)
    (local i32 i32 i64)
    i32.const 11
    i64.const 22
    local.get 0
    local.tee 1
    i32.const 42
    i32.eq
    if (type 0) (param i32 i64) (result i32 i64) ;; label = @1
      local.set 3
      local.set 2
      local.get 2
      i32.const 1
      i32.add
      local.get 3
      i64.const 1
      i64.add
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

// TODO FITZGEN: multiple callees for same call site w/ high enough ratio that is the same and how
// we break ties

// TODO FITZGEN: not high enough ratio

// TODO FITZGEN: inline a call site with call sites that we want to inline inside it

// TODO FITZGEN: too much inline depth

// TODO FITZGEN: recursion

// TODO FITZGEN: inline function with multiple locals

// TODO FITZGEN: multiple funcref tables

// TODO FITZGEN: bogus profile shows calls to func with wrong type

// TODO FITZGEN: bogus profile shows calls to func that doesn't exist (index oob)

// TODO FITZGEN: bogus profile with more call site data than actual call sites

// TODO FITZGEN: bogus profile with calls to unknown table elements
