use anyhow::{Context, Result};
use std::sync::atomic::{AtomicU32, Ordering};
use winliner::{InstrumentationStrategy, Instrumenter};

fn assert_instrumentation(
    instrumenter: Instrumenter,
    input: &str,
    expected_wat: &str,
) -> Result<()> {
    let _ = env_logger::try_init();

    let input = wat::parse_str(input).context("failed to parse test input as WAT")?;

    let actual_wasm = instrumenter
        .instrument(&input)
        .context("failed to instrument input Wasm")?;

    if log::log_enabled!(log::Level::Debug) {
        static TEST_CASE: AtomicU32 = AtomicU32::new(0);
        let n = TEST_CASE.fetch_add(1, Ordering::AcqRel);
        log::debug!("Writing instrumented Wasm to `instrumented{n}.wasm`");
        std::fs::write(format!("instrumented{n}.wasm"), &actual_wasm).unwrap();
    }

    let actual_wat =
        wasmprinter::print_bytes(&actual_wasm).context("failed to print output Wasm as WAT")?;

    let actual_wat = actual_wat.trim();
    println!("Expected:\n\n{expected_wat}\n\n");
    let expected_wat = expected_wat.trim();
    println!("Actual:\n\n{actual_wat}\n\n");

    wasmparser::Validator::new_with_features(wasmparser::WasmFeatures {
        function_references: true,
        ..Default::default()
    })
    .validate_all(&actual_wasm)
    .context("the instrumented wasm failed to validate")?;

    if expected_wat != actual_wat {
        panic!("expected != actual");
    }

    Ok(())
}

mod three_globals {
    use super::*;

    fn instrumenter() -> Instrumenter {
        let mut i = Instrumenter::new();
        i.strategy(InstrumentationStrategy::ThreeGlobals);
        i
    }

    #[test]
    fn empty_module() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module)
            "#,
            r#"
(module)
            "#,
        )
    }

    #[test]
    fn basic() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (type (func))
  (table 100 100 funcref)
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 1) (param i32)
    (local i32)
    local.get 0
    global.get 0
    i64.const 1
    i64.add
    global.set 0
    local.tee 1
    global.get 1
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 1
      i64.const 0
      global.set 2
    end
    global.get 2
    i64.const 1
    i64.add
    global.set 2
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (global (;0;) (mut i64) i64.const 0)
  (global (;1;) (mut i32) i32.const -1)
  (global (;2;) (mut i64) i64.const 0)
  (export "__winliner_call_site_0_total" (global 0))
  (export "__winliner_call_site_0_last_callee" (global 1))
  (export "__winliner_call_site_0_last_callee_count" (global 2))
)
            "#,
        )
    }

    #[test]
    fn multiple_call_sites() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (type (func))
  (table 100 100 funcref)
  (func (param i32)
    local.get 0
    call_indirect (type 0)
    local.get 0
    call_indirect (type 0)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 1) (param i32)
    (local i32)
    local.get 0
    global.get 0
    i64.const 1
    i64.add
    global.set 0
    local.tee 1
    global.get 1
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 1
      i64.const 0
      global.set 2
    end
    global.get 2
    i64.const 1
    i64.add
    global.set 2
    local.get 1
    call_indirect (type 0)
    local.get 0
    global.get 3
    i64.const 1
    i64.add
    global.set 3
    local.tee 1
    global.get 4
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 4
      i64.const 0
      global.set 5
    end
    global.get 5
    i64.const 1
    i64.add
    global.set 5
    local.get 1
    call_indirect (type 0)
    local.get 0
    global.get 6
    i64.const 1
    i64.add
    global.set 6
    local.tee 1
    global.get 7
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 7
      i64.const 0
      global.set 8
    end
    global.get 8
    i64.const 1
    i64.add
    global.set 8
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (global (;0;) (mut i64) i64.const 0)
  (global (;1;) (mut i32) i32.const -1)
  (global (;2;) (mut i64) i64.const 0)
  (global (;3;) (mut i64) i64.const 0)
  (global (;4;) (mut i32) i32.const -1)
  (global (;5;) (mut i64) i64.const 0)
  (global (;6;) (mut i64) i64.const 0)
  (global (;7;) (mut i32) i32.const -1)
  (global (;8;) (mut i64) i64.const 0)
  (export "__winliner_call_site_0_total" (global 0))
  (export "__winliner_call_site_0_last_callee" (global 1))
  (export "__winliner_call_site_0_last_callee_count" (global 2))
  (export "__winliner_call_site_1_total" (global 3))
  (export "__winliner_call_site_1_last_callee" (global 4))
  (export "__winliner_call_site_1_last_callee_count" (global 5))
  (export "__winliner_call_site_2_total" (global 6))
  (export "__winliner_call_site_2_last_callee" (global 7))
  (export "__winliner_call_site_2_last_callee_count" (global 8))
)
            "#,
        )
    }

    #[test]
    fn multiple_call_sites_across_functions() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (type (func))
  (table 100 100 funcref)
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 1) (param i32)
    (local i32)
    local.get 0
    global.get 0
    i64.const 1
    i64.add
    global.set 0
    local.tee 1
    global.get 1
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 1
      i64.const 0
      global.set 2
    end
    global.get 2
    i64.const 1
    i64.add
    global.set 2
    local.get 1
    call_indirect (type 0)
  )
  (func (;1;) (type 1) (param i32)
    (local i32)
    local.get 0
    global.get 3
    i64.const 1
    i64.add
    global.set 3
    local.tee 1
    global.get 4
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 4
      i64.const 0
      global.set 5
    end
    global.get 5
    i64.const 1
    i64.add
    global.set 5
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
  (global (;0;) (mut i64) i64.const 0)
  (global (;1;) (mut i32) i32.const -1)
  (global (;2;) (mut i64) i64.const 0)
  (global (;3;) (mut i64) i64.const 0)
  (global (;4;) (mut i32) i32.const -1)
  (global (;5;) (mut i64) i64.const 0)
  (export "__winliner_call_site_0_total" (global 0))
  (export "__winliner_call_site_0_last_callee" (global 1))
  (export "__winliner_call_site_0_last_callee_count" (global 2))
  (export "__winliner_call_site_1_total" (global 3))
  (export "__winliner_call_site_1_last_callee" (global 4))
  (export "__winliner_call_site_1_last_callee_count" (global 5))
)
            "#,
        )
    }

    #[test]
    fn no_call_indirect() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (elem funcref (ref.func 0) (ref.func 1))
  (func)
  (func)
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    (local i32)
  )
  (func (;1;) (type 0)
    (local i32)
  )
  (elem (;0;) funcref (ref.func 0) (ref.func 1))
)
            "#,
        )
    }

    #[test]
    fn disallow_table_set() -> Result<()> {
        let result = assert_instrumentation(
            instrumenter(),
            r#"
(module
  (table 10 10 funcref)
  (elem declare funcref (ref.func 0))
  (func
    i32.const 0
    ref.func 0
    table.set
  )
)
            "#,
            r#"
<fail>
            "#,
        );
        let err = result.expect_err("should have failed to instrument");
        println!("Actual error: {err:?}");
        assert!(format!("{err:?}").contains("Found table mutation instruction"));
        Ok(())
    }

    #[test]
    fn allow_table_set() -> Result<()> {
        let mut instrumenter = instrumenter();
        instrumenter.allow_table_mutation(true);
        assert_instrumentation(
            instrumenter,
            r#"
(module
  (table 10 10 funcref)
  (elem declare funcref (ref.func 0))
  (func
    i32.const 0
    ref.func 0
    table.set
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    (local i32)
    i32.const 0
    ref.func 0
    table.set 0
  )
  (table (;0;) 10 10 funcref)
  (elem (;0;) declare funcref (ref.func 0))
)
            "#,
        )
    }

    #[test]
    fn existing_globals() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (global i32 (i32.const 42))
  (table 10 10 funcref)
  (type (func))
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 1) (param i32)
    (local i32)
    local.get 0
    global.get 1
    i64.const 1
    i64.add
    global.set 1
    local.tee 1
    global.get 2
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 2
      i64.const 0
      global.set 3
    end
    global.get 3
    i64.const 1
    i64.add
    global.set 3
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 10 10 funcref)
  (global (;0;) i32 i32.const 42)
  (global (;1;) (mut i64) i64.const 0)
  (global (;2;) (mut i32) i32.const -1)
  (global (;3;) (mut i64) i64.const 0)
  (export "__winliner_call_site_0_total" (global 1))
  (export "__winliner_call_site_0_last_callee" (global 2))
  (export "__winliner_call_site_0_last_callee_count" (global 3))
)
            "#,
        )
    }

    #[test]
    fn existing_exports() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (table 10 10 funcref)
  (type (func))
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
  (export "foo" (func 0))
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (func (;0;) (type 1) (param i32)
    (local i32)
    local.get 0
    global.get 0
    i64.const 1
    i64.add
    global.set 0
    local.tee 1
    global.get 1
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 1
      global.set 1
      i64.const 0
      global.set 2
    end
    global.get 2
    i64.const 1
    i64.add
    global.set 2
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 10 10 funcref)
  (global (;0;) (mut i64) i64.const 0)
  (global (;1;) (mut i32) i32.const -1)
  (global (;2;) (mut i64) i64.const 0)
  (export "foo" (func 0))
  (export "__winliner_call_site_0_total" (global 0))
  (export "__winliner_call_site_0_last_callee" (global 1))
  (export "__winliner_call_site_0_last_callee_count" (global 2))
)
            "#,
        )
    }

    #[test]
    fn func_with_locals() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (table 10 10 funcref)
  (type (func (param i32)))
  (func (param i32) (local i32)
    local.get 0
    local.get 1
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func (param i32)))
  (func (;0;) (type 0) (param i32)
    (local i32 i32)
    local.get 0
    local.get 1
    global.get 0
    i64.const 1
    i64.add
    global.set 0
    local.tee 2
    global.get 1
    i32.eq
    i32.eqz
    if ;; label = @1
      local.get 2
      global.set 1
      i64.const 0
      global.set 2
    end
    global.get 2
    i64.const 1
    i64.add
    global.set 2
    local.get 2
    call_indirect (type 0)
  )
  (table (;0;) 10 10 funcref)
  (global (;0;) (mut i64) i64.const 0)
  (global (;1;) (mut i32) i32.const -1)
  (global (;2;) (mut i64) i64.const 0)
  (export "__winliner_call_site_0_total" (global 0))
  (export "__winliner_call_site_0_last_callee" (global 1))
  (export "__winliner_call_site_0_last_callee_count" (global 2))
)
            "#,
        )
    }
}

mod precise_host_calls {
    use super::*;

    fn instrumenter() -> Instrumenter {
        let mut i = Instrumenter::new();
        i.strategy(InstrumentationStrategy::PreciseHostCalls);
        i
    }

    #[test]
    fn empty_module() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module)
            "#,
            r#"
(module)
            "#,
        )
    }

    #[test]
    fn basic() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (type (func))
  (table 100 100 funcref)
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 2)))
  (func (;1;) (type 1) (param i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 0
    call 0
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
)
            "#,
        )
    }

    #[test]
    fn multiple_call_sites() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (type (func))
  (table 100 100 funcref)
  (func (param i32)
    local.get 0
    call_indirect (type 0)
    local.get 0
    call_indirect (type 0)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 2)))
  (func (;1;) (type 1) (param i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 0
    call 0
    local.get 1
    call_indirect (type 0)
    local.get 0
    local.tee 1
    i32.const 1
    call 0
    local.get 1
    call_indirect (type 0)
    local.get 0
    local.tee 1
    i32.const 2
    call 0
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
)
            "#,
        )
    }

    #[test]
    fn multiple_call_sites_across_functions() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (type (func))
  (table 100 100 funcref)
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
  (func (param i32)
    local.get 0
    call_indirect (type 0)
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 2)))
  (func (;1;) (type 1) (param i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 0
    call 0
    local.get 1
    call_indirect (type 0)
  )
  (func (;2;) (type 1) (param i32)
    (local i32)
    local.get 0
    local.tee 1
    i32.const 1
    call 0
    local.get 1
    call_indirect (type 0)
  )
  (table (;0;) 100 100 funcref)
)
            "#,
        )
    }

    #[test]
    fn no_call_indirect() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (elem funcref (ref.func 0) (ref.func 1))
  (func)
  (func)
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (func (;1;) (type 0)
    (local i32)
  )
  (func (;2;) (type 0)
    (local i32)
  )
  (elem (;0;) funcref (ref.func 1) (ref.func 2))
)
            "#,
        )
    }

    #[test]
    fn elem_segments() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (table 100 100 funcref)

  ;; Active.
  (elem (table 0) (i32.const 0) funcref (ref.func 0) (ref.null func))
  ;; Passive.
  (elem funcref (ref.func 0) (ref.null func))
  ;; Declared.
  (elem declare funcref (ref.func 0) (ref.null func))

  (func)
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (func (;1;) (type 0)
    (local i32)
  )
  (table (;0;) 100 100 funcref)
  (elem (;0;) (i32.const 0) funcref (ref.func 1) (ref.null func))
  (elem (;1;) funcref (ref.func 1) (ref.null func))
  (elem (;2;) declare funcref (ref.func 1) (ref.null func))
)
            "#,
        )
    }

    #[test]
    fn global_ref_func() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (func)
  (global funcref (ref.func 0))
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (func (;1;) (type 0)
    (local i32)
  )
  (global (;0;) funcref ref.func 1)
)
            "#,
        )
    }

    #[test]
    fn have_imported_func() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (import "foo" "bar" (func))
  (func
    call 0
  )
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (import "foo" "bar" (func (;1;) (type 0)))
  (func (;2;) (type 0)
    (local i32)
    call 1
  )
)
            "#,
        )
    }

    #[test]
    fn ref_func() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (elem declare funcref (ref.func 0))
  (func (result funcref)
    ref.func 0
  )
)
            "#,
            r#"
(module
  (type (;0;) (func (result funcref)))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (func (;1;) (type 0) (result funcref)
    (local i32)
    ref.func 1
  )
  (elem (;0;) declare funcref (ref.func 1))
)
            "#,
        )
    }

    #[test]
    fn start_section() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (start 0)
  (func)
)
            "#,
            r#"
(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (func (;1;) (type 0)
    (local i32)
  )
  (start 1)
)
            "#,
        )
    }

    #[test]
    fn func_with_locals() -> Result<()> {
        assert_instrumentation(
            instrumenter(),
            r#"
(module
  (table 10 10 funcref)
  (type (func (param i32)))
  (func (param i32) (local i32)
    local.get 0
    local.get 1
    call_indirect (type 0)
  )
  (export "foo" (func 0))
)
            "#,
            r#"
(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32 i32)))
  (import "winliner" "add_indirect_call" (func (;0;) (type 1)))
  (func (;1;) (type 0) (param i32)
    (local i32 i32)
    local.get 0
    local.get 1
    local.tee 2
    i32.const 0
    call 0
    local.get 2
    call_indirect (type 0)
  )
  (table (;0;) 10 10 funcref)
  (export "foo" (func 0))
)
            "#,
        )
    }
}
