# VLIW Benchmark

This benchmark is a Very-Long-Instruction-Word instruction scheduler
written by John Danskin at Princeton.  It is one of the original
**SML/NJ** benchmarks described in Appel's *Compiling with Continuations*.

This version of the benchmark has a number of significant changes (the
file `vliw.sml` has the original version of the benchmark).

* the program has been decomposed into separate files (one top-level
  compilation unit per file)

* the original version was written for a very early version of SML/NJ
  that use single character strings to represent characters.  While
  previous versions of this benchmark emulated the old behavior of
  functions like `implode` and `explode`, here we have switched to
  using the [**SML Basis Library**](https://smlfamily.github.io/Basis)
  APIs.

* We have also updated the I/O operations to use the `TextIO` structure.

* The other top-level, non-module, bindings have either been eliminated
  or relocated into the modules where they are used.  Thus, we can use
  CM to compile the program.

* The `Main` module has been rewritten to be more direct.
