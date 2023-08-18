//! Optimizing a Wasm program, assuming the behavior observed in the given
//! profile.

use crate::Profile;
use anyhow::Result;

#[cfg(feature = "clap")]
use clap::Parser;

/// TODO FITZGEN
#[cfg_attr(feature = "clap", derive(Parser))]
pub struct Optimizer {}

impl Optimizer {
    /// TODO FITZGEN
    pub fn optimize(&self, profile: &Profile) -> Result<Vec<u8>> {
        let _ = profile;
        todo!()
    }
}
