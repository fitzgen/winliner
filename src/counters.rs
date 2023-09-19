//! Reading the correct-versus-incorrect counters from an optimized instance.

use anyhow::{anyhow, ensure, Result};

/// Counters for how often speculative inlining guesses were correct or
/// incorrect.
///
/// After speculatively inlining a callee at a `call_indirect` site, you may
/// want to know whether you speculated correctly in practice or not. Is the
/// training set reflective of your real world workloads? Do your past recorded
/// profiles match current behavior?
///
/// This type counts how often each `call_indirect`'s target was correctly or
/// incorrectly guessed.
///
/// Construction of a `Counters` relies on the
/// [`emit_counters`][crate::Optimizer::emit_counters] option being enabled when
/// you generated the optimized Wasm. If they were not enabled, then you'll get
/// an empty set of counters.
///
/// ## Serializing and Deserializing `Counters`
///
/// When the `serde` cargo feature is enabled, `Counters` implements
/// `serde::Serialize` and `serde::Deserialize`:
///
/// ```
/// # fn foo() -> anyhow::Result<()> {
/// #![cfg(feature = "serde")]
///
/// use winliner::Counters;
///
/// // Read counters in from disk.
/// let file = std::fs::File::open("path/to/my/counters.json")?;
/// let my_counters: Counters = serde_json::from_reader(file)?;
///
/// // Write counters out to disk.
/// let file = std::fs::File::create("path/to/new/counters.json")?;
/// serde_json::to_writer(file, &my_counters)?;
/// # Ok(()) }
/// ```
#[derive(Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Counters {
    counters: Vec<Counter>,
    total_correct: u64,
    total_incorrect: u64,
}

/// How often a single speculative inlining call site was guessed correctly or
/// incorrectly.
///
/// See [`Counters`][crate::Counters] for more details.
#[derive(Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Counter {
    correct: u64,
    incorrect: u64,
}

impl Counters {
    /// Extract counters from an optimized Wasm program.
    ///
    /// The program must have been optimized by Winliner with the
    /// [`emit_counters`][crate::Optimizer::emit_counters] option enabled.
    ///
    /// To avoid a public dependency on any particular version of Wasmtime (or
    /// any other Wasm runtime for that matter) this method takes a callback
    /// function to read a global (by name) from a Wasm instance instead of
    /// taking the Wasm instance as a parameter directly. It is up to callers to
    /// implement this callback function for their Wasm runtime. The callback
    /// function must be able to read `i64`-typed Wasm globals.
    ///
    /// # Example
    ///
    /// ```
    /// # fn foo() -> wasmtime::Result<()> {
    /// use wasmtime::{Instance, Module, Store, Val};
    /// use winliner::Counters;
    ///
    /// // Instantiate your optimized Wasm module.
    /// let mut store = Store::<()>::default();
    /// let module = Module::from_file(store.engine(), "path/to/optimized.wasm")?;
    /// let instance = Instance::new(&mut store, &module, &[])?;
    ///
    /// // Run the Wasm instance, call its exports, etc...
    /// # let run = |_, _| -> wasmtime::Result<()> { Ok(()) };
    /// run(&mut store, instance)?;
    ///
    /// // Extract the counters from the instance.
    /// let counters = Counters::from_instance(|name| {
    ///     match instance.get_global(&mut store, name)?.get(&mut store) {
    ///         Val::I64(x) => Some(x as u64),
    ///         _ => None,
    ///     }
    /// })?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_instance(mut read_global: impl FnMut(&str) -> Option<u64>) -> Result<Self> {
        let mut counters = vec![];
        let mut total_correct = 0_u64;
        let mut total_incorrect = 0_u64;

        for i in 0.. {
            let correct = match read_global(&format!("__winliner_counter_{i}_correct")) {
                Some(x) => x,
                None => break,
            };
            total_correct = total_correct.saturating_add(correct);

            let incorrect =
                read_global(&format!("__winliner_counter_{i}_incorrect")).ok_or_else(|| {
                    anyhow!("Failed to read `__winliner_counter_{i}_incorrect` global")
                })?;
            total_incorrect = total_incorrect.saturating_add(incorrect);

            counters.push(Counter { correct, incorrect });
        }

        Ok(Counters {
            counters,
            total_correct,
            total_incorrect,
        })
    }

    /// Merge another set of counters into this one.
    ///
    /// The `other` counters are merged into `self`.
    ///
    /// # Example
    ///
    /// ```
    /// # fn foo() -> anyhow::Result<()> {
    /// use wasmtime::{Engine, Module};
    /// use winliner::Counters;
    ///
    /// // Load the optimized Wasm module.
    /// let engine = Engine::default();
    /// let module = Module::from_file(&engine, "path/to/optimized.wasm")?;
    ///
    /// // Run the Wasm a couple times.
    /// # let run_and_get_counters = |_| -> anyhow::Result<Counters> { unimplemented!() };
    /// let mut counters1 = run_and_get_counters(&module)?;
    /// let counters2 = run_and_get_counters(&module)?;
    ///
    /// // Finally, combine the two sets of counters into a single set.
    /// counters1.merge(&counters2);
    /// # Ok(()) }
    /// ```
    pub fn merge(&mut self, other: &Self) -> Result<()> {
        ensure!(
            self.counters.len() == other.counters.len(),
            "incompatible counters: generated from different Wasm modules"
        );

        for (me, them) in self.counters.iter_mut().zip(&other.counters) {
            me.correct = me.correct.saturating_add(them.correct);
            me.incorrect += me.incorrect.saturating_add(them.incorrect);
        }

        self.total_correct = self.total_correct.saturating_add(other.total_correct);
        self.total_incorrect = self.total_incorrect.saturating_add(other.total_incorrect);

        Ok(())
    }

    /// Get each counter in this set.
    ///
    /// You can use this to check for whether any speculative inlining has too
    /// high of an incorrect guess rate.
    pub fn counters(&self) -> &[Counter] {
        &self.counters
    }

    /// Get the total number of calls represented by this set of counters.
    pub fn total(&self) -> u64 {
        self.total_correct.saturating_add(self.total_incorrect)
    }

    /// Get the total number of times we correctly guessed the callee in this
    /// set of counters.
    pub fn total_correct(&self) -> u64 {
        self.total_correct
    }

    /// Get the total number of times we incorrectly guessed the callee in this
    /// set of counters.
    pub fn total_incorrect(&self) -> u64 {
        self.total_incorrect
    }

    /// Get the total ratio of correct guesses in this set of counters.
    pub fn total_correct_ratio(&self) -> f64 {
        self.total_correct as f64 / self.total() as f64
    }

    /// Get the total ratio of incorrect guesses in this set of counters.
    pub fn total_incorrect_ratio(&self) -> f64 {
        self.total_incorrect as f64 / self.total() as f64
    }
}

impl Counter {
    /// The number of times we guessed correctly for this speculative inlining.
    pub fn correct(&self) -> u64 {
        self.correct
    }

    /// The number of times we guessed incorrectly for this speculative
    /// inlining.
    pub fn incorrect(&self) -> u64 {
        self.incorrect
    }

    /// The total number of calls, correct or incorrect, to this counter's call
    /// site.
    pub fn total(&self) -> u64 {
        self.correct.saturating_add(self.incorrect)
    }

    /// The ratio of correct guesses.
    pub fn correct_ratio(&self) -> f64 {
        self.correct as f64 / self.total() as f64
    }

    /// The ratio of incorrect guesses.
    pub fn incorrect_ratio(&self) -> f64 {
        self.incorrect as f64 / self.total() as f64
    }
}
