use anyhow::{Context, Result};
use winliner::{InstrumentationStrategy, Instrumenter};

fn assert_instrumentation(
    instrumenter: Instrumenter,
    input: &str,
    expected_output: &str,
) -> Result<()> {
    let input = wat::parse_str(input).context("failed to parse test input as WAT")?;
    let actual_output = instrumenter
        .instrument(&input)
        .context("failed to instrument input Wasm")?;
    let actual_output =
        wasmprinter::print_bytes(&actual_output).context("failed to print output Wasm as WAT")?;
    assert_eq!(expected_output.trim(), actual_output.trim());
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
(module)
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
(module)
            "#,
        )
    }
}
