//! Winliner is an indirect call inliner for WebAssembly.

#![deny(missing_docs)]

mod convert;
mod instrument;
mod optimize;
mod profile;

pub use instrument::{InstrumentationStrategy, Instrumenter};
pub use optimize::Optimizer;
pub use profile::{Profile, ProfileBuilder};
pub use wasmtime;
