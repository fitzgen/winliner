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

#[test]
fn multiple_callees_that_satisfy_inlining_conditions() -> Result<()> {
    // Eventually we should support speculatively inlining multiple functions
    // (up to a configurable limit, of course) but until then test that we
    // choose the callee that was called the most.
    assert_optimize(
        Optimizer::new().min_ratio(0.1)?.min_total_calls(1),
        &[&[(10, 10), (20, 5)]],
        r#"
(module
  (type (func (result i32)))

  (func (type 0)
    i32.const 11
  )
  (func (type 0)
    i32.const 22
  )

  (func (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 10) funcref (ref.func 0))
  (elem (i32.const 20) funcref (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 0) (result i32)
    (local i32)
    i32.const 22
  )
  (func (;2;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 10
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 11
    else
      local.get 1
      call_indirect (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 10) funcref (ref.func 0))
  (elem (;1;) (i32.const 20) funcref (ref.func 1))
)
        "#,
    )
}

#[test]
fn multiple_callees_that_satisfy_inlining_conditions_with_tie_breaking() -> Result<()> {
    // Same as previous test but with callees that have been called the exact
    // same amount of times. Need to break ties deterministically.
    assert_optimize(
        Optimizer::new().min_ratio(0.1)?.min_total_calls(1),
        &[&[(10, 10), (20, 10)]],
        r#"
(module
  (type (func (result i32)))

  (func (type 0)
    i32.const 11
  )
  (func (type 0)
    i32.const 22
  )

  (func (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 10) funcref (ref.func 0))
  (elem (i32.const 20) funcref (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 0) (result i32)
    (local i32)
    i32.const 22
  )
  (func (;2;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 10
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 11
    else
      local.get 1
      call_indirect (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 10) funcref (ref.func 0))
  (elem (;1;) (i32.const 20) funcref (ref.func 1))
)
        "#,
    )
}

#[test]
fn not_high_enough_ratio() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1).min_ratio(1.0)?,
        &[&[(0, 1), (1, 9_999_999)]],
        r#"
(module
  (type (func (result i32)))

  (func (type 0)
    i32.const 11
  )
  (func (type 0)
    i32.const 22
  )

  (func (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 0) (result i32)
    (local i32)
    i32.const 22
  )
  (func (;2;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
    )
}

#[test]
fn dont_inline_direct_recursion() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1),
        &[&[(0, 1)]],
        r#"
(module
  (type (func (param i32) (result i32)))

  (func (type 0)
    local.get 0
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0))
)
        "#,
        r#"
(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0))
)
        "#,
    )
}

#[test]
fn multiple_funcref_tables() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1),
        &[&[(0, 1)], &[(0, 1)]],
        r#"
(module
  (type (func (result i32)))

  (func (type 0)
    i32.const 11
  )

  (func (type 0)
    i32.const 22
  )

  (func (param i32 i32) (result i32 i32)
    local.get 0
    call_indirect 0 (type 0)
    local.get 1
    call_indirect 1 (type 0)
  )

  (table 100 100 funcref)
  (elem (table 0) (i32.const 0) funcref (ref.func 0))

  (table 100 100 funcref)
  (elem (table 1) (i32.const 0) funcref (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32 i32) (result i32 i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 0) (result i32)
    (local i32)
    i32.const 22
  )
  (func (;2;) (type 1) (param i32 i32) (result i32 i32)
    (local i32)
    local.get 0
    local.tee 2
    i32.const 0
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 11
    else
      local.get 2
      call_indirect (type 0)
    end
    local.get 1
    local.tee 2
    i32.const 0
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 22
    else
      local.get 2
      call_indirect 1 (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (table (;1;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0))
  (elem (;1;) (table 1) (i32.const 0) funcref (ref.func 1))
)
        "#,
    )
}

#[test]
fn inlining_into_inlined_function() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1_000).max_inline_depth(2),
        &[&[(0, 1_000)], &[(1, 1_000)]],
        r#"
(module
  (type (func (result i32)))
  (type (func (param i32) (result i32)))

  (func (type 0)
    i32.const 11
  )

  (func (type 1)
    local.get 0
    call_indirect (type 0)
  )

  (func (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call_indirect (type 1)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 0
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 11
    else
      local.get 1
      call_indirect (type 0)
    end
  )
  (func (;2;) (type 2) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    local.tee 2
    i32.const 1
    i32.eq
    if (type 1) (param i32) (result i32) ;; label = @1
      local.set 3
      local.get 3
      local.tee 2
      i32.const 0
      i32.eq
      if (type 0) (result i32) ;; label = @2
        i32.const 11
      else
        local.get 2
        call_indirect (type 0)
      end
    else
      local.get 2
      call_indirect (type 1)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
    )
}

#[test]
fn reach_inline_depth_limit() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1_000).max_inline_depth(1),
        &[&[(0, 1_000)], &[(1, 1_000)]],
        r#"
(module
  (type (func (result i32)))
  (type (func (param i32) (result i32)))

  (func (type 0)
    i32.const 11
  )

  (func (type 1)
    local.get 0
    call_indirect (type 0)
  )

  (func (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call_indirect (type 1)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32)
    i32.const 11
  )
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 0
    i32.eq
    if (type 0) (result i32) ;; label = @1
      i32.const 11
    else
      local.get 1
      call_indirect (type 0)
    end
  )
  (func (;2;) (type 2) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    local.tee 2
    i32.const 1
    i32.eq
    if (type 1) (param i32) (result i32) ;; label = @1
      local.set 3
      local.get 3
      call_indirect (type 0)
    else
      local.get 2
      call_indirect (type 1)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
    )
}

#[test]
fn mutual_recursion() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(100).max_inline_depth(100),
        &[&[(1, 100)], &[(0, 100)]],
        r#"
(module
  (type (func (param i32 i32) (result i32)))

  (func (type 0)
    local.get 0
    local.get 1
    local.get 0
    call_indirect (type 0)
  )

  (func (type 0)
    local.get 0
    local.get 1
    local.get 1
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    local.get 1
    local.get 0
    local.tee 2
    i32.const 1
    i32.eq
    if (type 0) (param i32 i32) (result i32) ;; label = @1
      local.set 4
      local.set 3
      local.get 3
      local.get 4
      local.get 4
      call_indirect (type 0)
    else
      local.get 2
      call_indirect (type 0)
    end
  )
  (func (;1;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    local.get 1
    local.get 1
    local.tee 2
    i32.const 0
    i32.eq
    if (type 0) (param i32 i32) (result i32) ;; label = @1
      local.set 4
      local.set 3
      local.get 3
      local.get 4
      local.get 3
      call_indirect (type 0)
    else
      local.get 2
      call_indirect (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
    )
}

#[test]
fn inline_a_function_with_many_locals() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(100),
        &[&[(0, 100)]],
        r#"
(module
  (type (func (result i32)))

  (func (type 0) (local i32 i64 f32 f64 v128 externref funcref)
    local.get 0
  )

  (func (param i32) (result i32)
    (local funcref externref v128 f64 f32 i64 i32)
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
        r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32 i64 f32 f64 v128 externref funcref i32)
    local.get 0
  )
  (func (;1;) (type 1) (param i32) (result i32)
    (local funcref externref v128 f64 f32 i64 i32 i32 i32 i64 f32 f64 v128 externref funcref)
    local.get 0
    local.tee 8
    i32.const 0
    i32.eq
    if (type 0) (result i32) ;; label = @1
      local.get 9
    else
      local.get 8
      call_indirect (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0) (ref.func 1))
)
        "#,
    )
}

#[test]
fn counters() -> Result<()> {
    assert_optimize(
        Optimizer::new().min_total_calls(1).emit_counters(true),
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
      global.get 0
      i64.const 1
      i64.add
      global.set 0
      i32.const 36
    else
      global.get 1
      i64.const 1
      i64.add
      global.set 1
      local.get 1
      call_indirect (type 0)
    end
  )
  (table (;0;) 100 100 funcref)
  (global (;0;) (mut i64) i64.const 0)
  (global (;1;) (mut i64) i64.const 0)
  (export "__winliner_counter_0_correct" (global 0))
  (export "__winliner_counter_0_incorrect" (global 1))
  (elem (;0;) (i32.const 42) funcref (ref.func 0))
)
        "#,
    )
}

/// Tests for when we run optimization with bogus profiles.
///
/// The exact behavior doesn't matter too much here (we do document the
/// requirement that the profile is valid for the input Wasm) as long as we
/// don't panic and are deterministic.
mod bogus_profile {
    use super::*;

    #[test]
    fn call_to_func_of_wrong_type() -> Result<()> {
        // Note that this one could actually happen where we called the function
        // and then it trapped, and if we merged multiple profiles we could even
        // see many calls to a function of the wrong type. We could technically
        // even speculatively trap here, but it doesn't gain us anything.
        assert_optimize(
            Optimizer::new().min_total_calls(1),
            &[&[(0, 1)]],
            r#"
(module
  (type (func (result i32)))

  (func (param i32) (result i32)
    local.get 0
  )

  (func (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )

  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0))
)
            "#,
            r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
  )
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0))
)
            "#,
        )
    }

    #[test]
    fn call_to_out_of_bounds_function_index() -> Result<()> {
        assert_optimize(
            Optimizer::new().min_total_calls(1),
            &[&[(999, 1)]],
            r#"
(module
  (type (func (result i32)))
  (func (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )
  (table 100 100 funcref)
)
            "#,
            r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
)
            "#,
        )
    }

    #[test]
    fn more_call_site_data_than_call_sites() -> Result<()> {
        assert_optimize(
            Optimizer::new().min_total_calls(1),
            &[&[(0, 999)]],
            r#"
(module
  (func (param i32) (result i32)
    local.get 0
  )
  (table 100 100 funcref)
  (elem (i32.const 0) funcref (ref.func 0))
)
            "#,
            r#"
(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 0))
)
            "#,
        )
    }

    #[test]
    fn call_to_unknown_table_element() -> Result<()> {
        assert_optimize(
            Optimizer::new().min_total_calls(1),
            &[&[(0, 1)]],
            r#"
(module
  (type (func (result i32)))
  (func (param i32) (result i32)
    local.get 0
    call_indirect (type 0)
  )
  (table 100 100 funcref)
  (elem (i32.const 1) funcref (ref.func 0))
)
            "#,
            r#"
(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (func (;0;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 1) funcref (ref.func 0))
)
            "#,
        )
    }
}
