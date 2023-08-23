#![doc = include_str!("../README.md")]

#![deny(missing_docs)]

mod convert;
mod instrument;
mod optimize;
mod profile;

pub use instrument::{InstrumentationStrategy, Instrumenter};
pub use optimize::Optimizer;
pub use profile::{Profile, ProfileBuilder};
