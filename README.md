# SML/NJ Benchmark Suite

This repository contains benchmark programs and supporting script
for measuring the performance of the SML/NJ system.  These include
the "classic" benchmarks that have frequently been used to evaluate
the performance of SML/NJ (and other SML systems) in the published
literature.  The original version of this suite was assembled by
Lal George at Bell Laboratories in the late 1990s.

This version of the benchmark suite adds many additional programs,
increases the problem sizes for some programs, and modernizes the
code for others.  Where it make sense, we have broken up monolithic
programs into multiple modules contained in separate files.  The
benchmarking script provides a mechanism for re-combining these
modules into a single compilation unit.

## Running the Benchmarks

The `bench` script in the `bin` directory supports various commands
for running the benchmarks.

> bin/bench -h

will show the list of commands, and

> bin/bench <cmd> -h

will show the options for `<cmd>`.

## Benchmark Directory Structure

Each benchmark directory contains the following files (not all of which
may be present):

* `main.sml` -- a SML file that defines the `Main` structure; this structure
  has the `BMARK` signature.

* `sources.cm` -- a CM file for building the benchmark program.

* additional SML source files for the benchmark.

* `FILES` -- an optional list of additional source files (other than
  `main.sml`) in compilation order.  The `--single-file` option
  uses this list to construct a single-file version of the program.
  This file is omitted for benchmarks that have `main.sml` as their
  only source file.

* `ANSWER` -- an optional text file containing the correct output from
  running the `Main.testit` function.  Some benchmarks currently do not
  have test output; for these benchmarks, the `ANSWER` file is omitted.
  The `BMARK` signature includes a `results` field that is a list of
  additional outputs produced by the program (e.g., an image produced by
  a ray tracer).

* `DATA` -- an optional directory containing any input data files required
  by the benchmark program.

## The Benchmark Programs

The following is a brief description of each benchmark program.
The programs are listed by directory name in alphabetical order;
for more detailed information, see the `README.md` file in each
directory.

Benchmark programs that are labeled as "**(BROKEN)**" in the table have either not been
ported to the benchmarking infrastructure, or have bugs (see the [Issues](
https://github.com/smlnj/benchmarks/issues) for details).

| Directory                                             | LOC   | Short Description                          |
| ----------------------------------------------------- | ----- | ------------------------------------------ |
| [`aobench`](programs/aobench/README.md)               |   252 | A small ambient occlusion renderer         |
| [`barnes-hut`](programs/barnes-hut/README.md)         |   820 | Hierarchical N-body solver **(BROKEN)**    |
| [`binary-trees`](programs/binary-trees/README.md)     |    48 | GC stress test                             |
| [`black-scholes`](programs/black-scholes/README.md)   |   128 | European-style option pricing              |
| [`boyer`](programs/boyer/README.md)                   |   854 | Tautology checker                          |
| [`cml-sieve`](programs/cml-sieve/README.md)           |   162 | Message-passing implementation of Sieve of Eratosthenes |
| [`count-graphs`](programs/count-graphs/README.md)     |   401 | Enumerate directed graphs                  |
| [`delta-blue`](programs/delta-blue/README.md)         |   703 | Delta-Blue incremental constraint solver **(BROKEN)** |
| [`dlx`](programs/dlx/README.md)                       | 1,887 | DLX simulator **(BROKEN)**                 |
| [`elimination`](programs/elimination/README.md)       | 5,135 | Model-elimination-based prover             |
| [`fannkuch`](programs/fannkuch/README.md)             |    72 | Array permutations                         |
| [`f-arith`](programs/f-arith/README.md)               |    25 | Calculation of pi using Maclaurin series   |
| [`fft`](programs/fft/README.md)                       |   166 | Fast-Fourier-transform implementation      |
| [`id-ray`](programs/id-ray/README.md)                 |   409 | A ray tracer ported from Id                |
| [`iter-pidigits`](programs/iter-pidigits/README.md)   |    53 | Computation of digits of pi                |
| [`kcfa`](programs/kcfa/README.md)                     |   xxx | A reference implementation of kCFA **(BROKEN)** |
| [`knuth-bendix`](programs/knuth-bendix/README.md)     |   434 | Knuth-Bendix completion algorithm          |
| [`lexgen`](programs/lexgen/README.md)                 | 1,033 | Lexical-analyzer generator                 |
| [`life`](programs/life/README.md)                     |   123 | Conway's game of life                      |
| [`logic`](programs/logic/README.md)                   |   323 |                                            |
| [`mandelbrot`](programs/mandelbrot/README.md)         |    50 | Mandelbrot-set computation                 |
| [`mandelbrot-rat`](programs/mandelbrot-rat/README.md) |    93 | Mandelbrot-set computation using rationals |
| [`mazefun`](programs/mazefun/README.md)               |   181 | Maze generator                             |
| [`mc-ray`](programs/mc-ray/README.md)                 |   737 | Monte-Carlo ray tracer                     |
| [`minimax`](programs/minimax/README.md)               |   168 | Minimax game-tree search for Tic-Tac-Toe   |
| [`mlyacc`](programs/mlyacc/README.md)                 | 5,573 | LALR(1) parser generator                   |
| [`nbody`](programs/nbody/README.md)                   |   136 | $O(n^2)$ N-body solver                     |
| [`nucleic`](programs/nucleic/README.md)               | 2,920 | Pseudoknot application from molecular biology |
| [`pia`](programs/pia/README.md)                       |   xxx | **(BROKEN)**                               |
| [`pidigits`](programs/pidigits/README.md)             |    77 | Stream-based computation of digits of pi   |
| [`pingpong`](programs/pingpong/README.md)             |   136 | Message-passing microbenchmark             |
| [`plclub-ray`](programs/plclub-ray/README.md)         | 2,025 | A raytracer from the ICFP'20 Programming Contest |
| [`professor`](programs/professor/README.md)           |   220 | A solver for a simple puzzle game          |
| [`ratio-regions`](programs/ratio-regions/README.md)   |   486 | Image segmentation/contour finding         |
| [`ray`](programs/ray/README.md)                       |   352 | A minimal ray tracer                       |
| [`regex`](programs/regex/README.md)                   |   xxx | **(BROKEN)**                               |
| [`safe-for-space`](programs/safe-for-space/README.md) |    33 | Test of closure-conversion space safety    |
| [`sat`](programs/sat/README.md)                       |    54 | Brute-force SAT solver                     |
| [`simple`](programs/simple/README.md)                 |   769 | A spherical fluid-dynamics program         |
| [`smith-nf`](programs/smith-nf/README.md)             |   372 | Computes the Smith Normal Form             |
| [`stream-sieve`](programs/sieve/README.md)            |    58 | Functional stream implementation of Sieve of Eratosthenes |
| [`tsp`](programs/tsp/README.md)                       |   303 | A Traveling-Sales-Person solver            |
| [`twenty-four`](programs/twenty-four/README.md)       |   120 | CPS-style solver for the 24 puzzle         |
| [`tyan`](programs/tyan/README.md)                     |   500 | Grobner Basis calculation                  |
| [`vliw`](programs/vliw/README.md)                     | 2,940 | A Very-Long-Instruction-Word instruction scheduler |
| [`zebra`](programs/zebra/README.md)                   |   205 | A solver for a word puzzle                 |
| [`zern`](programs/zern/README.md)                     |   502 | A BlurFit routine using the the Levenberg-Marquardt algorithm |

### Benchmark Classes

Each benchmark program is tagged with one or more "classes".  These classes are
used to easily specify a subset of the benchmarks.  For example,

```console
bin/bench run fp
```

will run the benchmarks that use floating-point arithmetic.  A list of the benchmark
classes is available with the command

```console
bin/bench list --classes
```

## Data File Format

The benchmarking framework produces its results as JSON files by default.
Each file typically contains an object representing one or more experiments.
The object has a number of common fields that describe the experimental
set up:

* `date` -- this field contains the date of the measurements in the format
  "`YYYY-MM-DD`".  When files are merged, the more recent date is retained.

* `machine` -- this field is a JSON object that describes the properties of
  the machine used to collect the measurements.

* `sml-command` -- the absolute path to the SML command used to run the experiments.

* `sml-system` -- the Standard ML system being measured.

* `sml-options` -- an array of control-settings for the compiler.

* `single-file` -- a boolean that specifies if the `--single-file` option
  was specified.

* `include-basis` -- a boolean that specifies if the `--include-basis` option
  was specified.

* `modes` -- an array of strings specifying the different measurements
  reported in the file.  The items in this list can have the following values:
    - `"run"` -- execution-time measurements
    - `"gc"` -- garbage-collector measurements
    - `"compile"` -- compile-time measurements
    - `"run-w-stats"` -- execution timing measurements with computed statistics
    - `"compile-w-stats"` -- execution-time measurements with computed statistics

* `alloc` -- the specified size of the allocation space.

In addition, there is a `data` field that is a list of objects, one per Benchmark.
Each of these objects contains a `program` field that holds the name of the
benchmark, plus one or more additional fields that hold measurements or summary
information.  The possible additional fields are:

* `run-times` -- this field is an array of execution-time measurements.
  Each measurement is a JSON object with the following fields:
    - `real` -- the total real (wall clock) time for the run
    - `total` -- the total (cpu plus gc) timing information
    - `cpu` -- the total cpu (aka non-GC) timing information
    - `gc` -- the tital GC timing information
  Timing information is represented as a JSON object with three fields:
    - `t` -- the total time (_i.e._, the sum of the other two fields)
    - `u` -- the reported user-space CPU time
    - `s` -- the reported system CPU time

* `gc-stats` -- this field is a JSON object with counting statistics for the
  garbage collector.  Its fields are
    - `nursery-alloc` -- the number of bytes of allocation in the nursery
    - `num-stores` -- an optional field that specifies the number of store-list
      objects allocated
    - `gen1-alloc` -- the number of bytes of allocation in the first generation
    - `gen1-promote` -- the number of bytes of data promoted from the
    - `num-gcs` -- an array of integers that reports the number of collections of
      each generation.  The first entry in the array is the number of minor
      collections.

* `compile-times` -- this field is an array of compile-time measurements.
  Each measurement is a JSON object with the same fields as the `run-times`
  objects described above.

* `run-time-stats` -- this is a JSON object with summary information computed
  from the `run-times` data.

* `compile-time-stats` -- this is a JSON object with summary information computed
  from the `compile-times` data.
