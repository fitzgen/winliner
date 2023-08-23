//! Optimizing a Wasm program, assuming the behavior observed in the given
//! profile.

use crate::Profile;
use anyhow::{bail, ensure, Result};

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
///     .min_ratio(0.99);
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
}

impl Default for Optimizer {
    fn default() -> Self {
        Optimizer {
            min_total_calls: 1000,
            min_ratio: 0.9,
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

    /// Optimize the given Wasm binary.
    ///
    /// Callers must ensure that:
    ///
    /// 1. The given Wasm must be the original, uninstrumented Wasm program.
    ///
    /// 2. The profile must have been created from an instrumented version of
    ///    this Wasm program.
    ///
    /// Failure to satisfy these requirements may result in an optimized Wasm
    /// binary that has divergent behavior from the original Wasm program.
    pub fn optimize(&self, profile: &Profile, wasm: &[u8]) -> Result<Vec<u8>> {
        // NB: Have to re-validate because the `clap`-parsed values aren't
        // validated upon construction.
        if self.min_ratio < 0.0 || 1.0 < self.min_ratio {
            bail!("The `--min-ratio` value must be between 0.0 and 1.0");
        }

        let _ = (profile, wasm);
        todo!()
    }
}
