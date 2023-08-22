//! Extracting profiles from instrumented Wasm programs and merging profiles
//! together.

use std::collections::BTreeMap;

use anyhow::Result;

/// TODO FITZGEN
#[derive(Clone)]
pub struct Profile {
    // Per-call site profiling information.
    //
    // Note that a lack of profile data for a particular call site implies that
    // the associated `call_indirect` was never executed.
    call_sites: BTreeMap<u32, CallSiteProfile>,
}

#[derive(Clone, Default)]
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
    //
    // TODO FITZGEN: dep inject here to avoid a public dep on wasmtime?
    pub fn from_three_globals(
        store: impl wasmtime::AsContext,
        instance: wasmtime::Instance,
    ) -> Result<Self> {
        let _ = (store, instance);
        todo!()
    }

    /// TODO FITZGEN
    pub fn merge(&mut self, other: &Profile) {
        let _ = other;
        todo!()
    }
}

/// TODO FITZGEN
pub struct ProfileBuilder {
    profile: Profile,
}

impl ProfileBuilder {
    /// Record the observed target of an indirect call at the given call site.
    pub fn add_indirect_call(&mut self, callee: u32, call_site: u32) {
        let call_site = self.profile.call_sites.entry(call_site).or_default();
        call_site.total_call_count += 1;
        *call_site.callee_to_count.entry(callee).or_default() += 1;
    }

    /// Construct the finished profile from this builder.
    pub fn build(&self) -> Profile {
        self.profile.clone()
    }
}
