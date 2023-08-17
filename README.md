<div align="center">
  <h1>Winliner</h1>

  <p>
    <strong>The WebAssembly indirect call inliner!</strong>
  </p>

  <p>
    <a href="https://github.com/fitzgen/winliner/actions?query=workflow%3ACI"><img src="https://github.com/bytecodealliance/wizer/workflows/CI/badge.svg" alt="build status" /></a>
    <a href="https://docs.rs/wizer"><img src="https://docs.rs/wizer/badge.svg" alt="Documentation Status" /></a>
  </p>

  <h3>
    <a href="https://docs.rs/winliner">API Docs</a>
    <span> | </span>
    <a href="https://github.com/fitzgen/winliner/blob/main/CONTRIBUTING.md">Contributing</a>
  </h3>
</div>

* [About](#about)
* [Install](#install)
* [Example Usage](#example-usage)
* [Caveats](#caveats)
* [Using Winliner as a Library](#using-winliner-as-a-library)

## About

Winliner speculatively inlines indirect calls in WebAssembly, based on observed
information from a previous profiling phase. This is a form of [profile-guided
optimization] that we affectionately call *Winlining*.

[profile-guided optimization]: https://en.wikipedia.org/wiki/Profile-guided_optimization

First, Winliner inserts instrumentation to observe the actual target callee of
every indirect call site in your Wasm program. Next, you run the instrumented
program for a while, building up a profile. Finally, you invoke Winliner again,
this time providing it with the recorded profile, and it optimizes your Wasm
program based on the behavior observed in that profile.

For example, if profiling shows that an indirect call always (or nearly always)
goes to the 42nd entry in the funcrefs table, then Winliner will perform the
following semantically-transparent transformation:

```wat
;; Before:

call_indirect

;; After:

;; If the callee index is 42, execute the inlined body of
;; the associated function.
local.tee $temp
i32.const 42
i32.eq
if
  <inlined body of table[42] here>
else
  local.get $temp
  call_indirect
end
```

The speculative inlining by itself is not a huge performance win, since CPU
indirect branch prediction is very powerful these days. However, it allows the
Wasm compiler to "see through" the indirect call and perform subsequent
optimizations (like [GVN] and [LICM]) on the inlined callee's body, which can
result in significant performance benefits.

[GVN]: https://en.wikipedia.org/wiki/Value_numbering#Global_value_numbering
[LICM]: https://en.wikipedia.org/wiki/Loop-invariant_code_motion

This technique is similar to *devirtualization* but doesn't require that the
compiler is able to statically determine the callee, nor that the callee is
always a single, particular function 100% of the time. Unlike devirtualization,
Winlining can still optimize indirect calls that go a certain way 99% of the
time and a different way 1% of the time because it can always fall back to an
unoptimized indirect call.

## Install

You can install via `cargo`:

```shell-session
$ cargo install winliner --all-features
```

## Example Usage

TODO

## Caveats

* Winliner is not safe in the face of mutations to the `funcref` table, which is
  possible via the `table.set` instruction (and others) introduced as part of
  [the reference-types
  proposal](https://github.com/WebAssembly/reference-types). You must either
  disable this proposal or manually uphold the invariant that the `funcref`
  table is never mutated. Breaking this invariant will likely lead to diverging
  behavior from the original program and very wonky bugs!

* Winliner cannot optimize `call_ref` instructions because WebAssembly function
  references are not comparable, so we can't insert the `if callee ==
  speculative_callee` check.

## Using Winliner as a Library

TODO
