//! Extracting profiles from instrumented Wasm programs and merging profiles
//! together.

use std::collections::BTreeMap;

use anyhow::{anyhow, ensure, Context, Result};

/// Observed behavior about one or more Wasm executions.
///
/// A `Profile` records observed `call_indirect` behavior about one or more Wasm
/// executions:
///
/// * How many times was each `call_indirect` executed?
/// * How many times was `table[x]` called from each call site?
/// * Etc...
///
/// ## Constructing a `Profile`
///
/// There are two primary ways to get a `Profile`, one for each instrumentation
/// strategy:
///
/// 1. If you instrumented your Wasm using the
/// [`InstrumentationStrategy::ThreeGlobals`][crate::InstrumentationStrategy::ThreeGlobals]
/// strategy, you can use the [`Profile::from_three_globals`] constructor.
///
/// 2. If you instrumented your Wasm using the
/// [`InstrumentationStrategy::PreciseHostCalls`][crate::InstrumentationStrategy::PreciseHostCalls]
/// strategy, you can implement the `winliner.add_indirect_call` host import
/// using a [`ProfileBuilder`][crate::ProfileBuilder] and then call
/// [`ProfileBuilder::build`][crate::ProfileBuilder::build] to extract the
/// finished profile.
///
/// ## Merging `Profile`s
///
/// It can be difficult to get representative profiling data from a single Wasm
/// execution. Luckily, a single `Profile` can represent many different
/// executions! For each profiling run, record a new `Profile` and then call
/// [`Profile::merge`] to combine them into a single, aggregate `Profile`.
///
/// ## Serializing and Deserializing `Profile`s
///
/// When the `serde` cargo feature is enabled, `Profile` implements
/// `serde::Serialize` and `serde::Deserialize`.
#[derive(Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Profile {
    // Per-call site profiling information.
    //
    // Note that a lack of profile data for a particular call site implies that
    // the associated `call_indirect` was never executed.
    call_sites: BTreeMap<u32, CallSiteProfile>,
}

#[derive(Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct CallSiteProfile {
    // The total count of indirect calls for this call site.
    total_call_count: u64,
    // The observed callees and their associated counts. Note that these counts
    // don't necessarily add up to `total_call_count` since we can be missing
    // information due to imprecise instrumentation strategies.
    callee_to_count: BTreeMap<u32, u64>,
}

impl Profile {
    /// Extract a profile from a Wasm program that was instrumented with the
    /// "three-globals" strategy.
    ///
    /// To avoid a public dependency on any particular version of Wasmtime (or
    /// any other Wasm runtime for that matter) this method takes a callback
    /// function to read a global (by name) from a Wasm instance instead of
    /// taking the Wasm instance as a parameter directly. It is up to callers to
    /// implement this callback function for their Wasm runtime. The callback
    /// function must be able to read `i32`- and `i64`-typed Wasm globals,
    /// zero-extending `i32` values as necessary.
    ///
    /// # Example
    ///
    /// ```
    /// # fn foo() -> wasmtime::Result<()> {
    /// use wasmtime::{Instance, Module, Store, Val};
    /// use winliner::Profile;
    ///
    /// // Instantiate your instrumented Wasm module.
    /// let mut store = Store::<()>::default();
    /// let module = Module::from_file(store.engine(), "path/to/instrumented.wasm")?;
    /// let instance = Instance::new(&mut store, &module, &[])?;
    ///
    /// // Run the Wasm instance, call its exports, etc... to gather PGO data.
    /// # let run = |_| -> wasmtime::Result<()> { Ok(()) };
    /// run(instance)?;
    ///
    /// // Extract the profile from the instance.
    /// let profile = Profile::from_three_globals(|name| {
    ///     match instance.get_global(&mut store, name)?.get(&mut store) {
    ///         Val::I32(x) => Some(x as u32 as u64),
    ///         Val::I64(x) => Some(x as u64),
    ///         _ => None,
    ///     }
    /// })?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_three_globals(mut read_global: impl FnMut(&str) -> Option<u64>) -> Result<Self> {
        let mut profile = Profile::default();

        for call_site_index in 0.. {
            let total_call_count =
                match read_global(&format!("__winliner_call_site_{call_site_index}_total")) {
                    None => break,
                    Some(x) => x,
                };

            let last_callee = read_global(&format!(
                "__winliner_call_site_{call_site_index}_last_callee"
            ))
            .ok_or_else(|| {
                anyhow!(
                    "Failed to read `__winliner_call_site_{call_site_index}_last_callee` global"
                )
            })?;
            let last_callee = u32::try_from(last_callee).context("callee is out of bounds")?;

            let last_callee_count = read_global(&format!(
                "__winliner_call_site_{call_site_index}_last_callee_count"
            ))
            .ok_or_else(|| {
                anyhow!(
                    "Failed to read `__winliner_call_site_{call_site_index}_last_callee` global"
                )
            })?;

            ensure!(
                total_call_count >= last_callee_count,
                "Bogus profiling data: call site's total count is less than the last callee's call \
                 count",
            );

            let mut callee_to_count = BTreeMap::new();
            callee_to_count.insert(last_callee, last_callee_count);

            profile.call_sites.insert(
                call_site_index,
                CallSiteProfile {
                    total_call_count,
                    callee_to_count,
                },
            );
        }

        Ok(profile)
    }

    /// Merge two profiles together.
    ///
    /// The `other` profile is merged into `self`.
    ///
    /// # Example
    ///
    /// ```
    /// # fn foo() -> anyhow::Result<()> {
    /// use wasmtime::{Engine, Module};
    /// use winliner::Profile;
    ///
    /// // Load the instrumented Wasm module.
    /// let engine = Engine::default();
    /// let module = Module::from_file(&engine, "path/to/instrumented.wasm")?;
    ///
    /// // Record a couple of PGO profiles.
    /// # let record_one_profile = |_| -> anyhow::Result<Profile> { unimplemented!() };
    /// let mut profile1 = record_one_profile(&module)?;
    /// let profile2 = record_one_profile(&module)?;
    ///
    /// // Finally, combine the two profiles into a single profile.
    /// profile1.merge(&profile2);
    /// # Ok(()) }
    /// ```
    pub fn merge(&mut self, other: &Profile) {
        for (call_site_index, other) in other.call_sites.iter() {
            let call_site = self.call_sites.entry(*call_site_index).or_default();
            call_site.total_call_count += other.total_call_count;
            for (callee, count) in other.callee_to_count.iter() {
                *call_site.callee_to_count.entry(*callee).or_default() += count;
            }
        }
    }
}

/// A builder for constructing [`Profile`][crate::Profile]s.
///
/// Primarily for use in conjunction with
/// [`InstrumentationStrategy::PreciseHostCalls`][crate::InstrumentationStrategy::PreciseHostCalls]
/// and implementing the `winliner.add_indirect_call` import function for the
/// instrumented Wasm.
///
/// # Example
///
/// ```
/// use winliner::ProfileBuilder;
///
/// // Create a new builder.
/// let mut builder = ProfileBuilder::new();
///
/// // Record some observed calls.
/// let callee = 42;
/// let call_site = 36;
/// builder.add_indirect_call(callee, call_site);
///
/// // Construct the finished profile from the builder.
/// let profile = builder.build();
/// ```
#[derive(Clone, Default)]
pub struct ProfileBuilder {
    profile: Profile,
}

impl ProfileBuilder {
    /// Create a new, empty builder.
    pub fn new() -> Self {
        Default::default()
    }

    /// Record the observed target of an indirect call at the given call site.
    pub fn add_indirect_call(&mut self, callee: u32, call_site: u32) {
        let call_site = self.profile.call_sites.entry(call_site).or_default();
        call_site.total_call_count += 1;
        *call_site.callee_to_count.entry(callee).or_default() += 1;
    }

    /// Construct the finished profile from this builder.
    pub fn build(self) -> Profile {
        self.profile
    }
}
