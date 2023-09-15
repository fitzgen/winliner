#![doc = include_str!("../README.md")]
#![deny(missing_docs)]

mod convert;
mod cow_section;
mod instrument;
mod optimize;
mod profile;

pub use anyhow::Result;
pub use instrument::{InstrumentationStrategy, Instrumenter};
pub use optimize::Optimizer;
pub use profile::{Profile, ProfileBuilder};
