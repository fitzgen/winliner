//! Fuzz the instrumentation pass.

#![no_main]

use libfuzzer_sys::{
    arbitrary::{self, Arbitrary},
    fuzz_target,
};
use winliner::{InstrumentationStrategy, Instrumenter};

#[derive(Arbitrary, Debug)]
struct InstrumenterOptions {
    allow_table_mutation: bool,
    allow_arbitrary_element_offsets: bool,
    allow_table_imports: bool,
    three_globals: bool,
}

#[derive(Arbitrary, Debug)]
struct FuzzInput {
    module: wasm_smith::ConfiguredModule<wasm_smith::SwarmConfig>,
    instrumenter: InstrumenterOptions,
}

impl FuzzInput {
    fn instrumenter(&self) -> Instrumenter {
        let mut i = Instrumenter::new();
        i.allow_table_mutation(self.instrumenter.allow_table_mutation);
        i.allow_arbitrary_element_offsets(self.instrumenter.allow_arbitrary_element_offsets);
        i.allow_table_imports(self.instrumenter.allow_table_imports);
        i.strategy(if self.instrumenter.three_globals {
            InstrumentationStrategy::ThreeGlobals
        } else {
            InstrumentationStrategy::HostCalls
        });
        i
    }
}

fuzz_target!(|input: FuzzInput| {
    let wasm = input.module.module.to_bytes();
    let instrumenter = input.instrumenter();
    let _ = instrumenter.instrument(&wasm);
});
