# SML/NJ Benchmark Suite

This repository contains benchmark programs and supporting script
for measuring the performance of the SML/NJ system.  These include
the "classic" benchmarks that have frequently been used to evaluate
the performance of SML/NJ (and other SML systems) in the published
literature.  The original version of this suite was assembled by
Lal George at Bell Laboratories in the late 1990s.

This version of the benchmark suite adds many additional programs,
increases the problem sizes for some programs, and modernizes the
code for others.

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
  `main.sml`) in compilation order.  The `bin/make-single-file.sh` script
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
| [`aobench`](programs/aobench/README.md)               |   259 | A small ambient occlusion renderer         |
| [`barnes-hut`](programs/barnes-hut/README.md)         |   820 | Hierarchical N-body solver **(BROKEN)**    |
| [`binary-trees`](programs/binary-trees/README.md)     |    xx | GC stress test                             |
| [`black-scholes`](programs/black-scholes/README.md)   |   128 | European-style option pricing              |
| [`boyer`](programs/boyer/README.md)                   |   854 | Tautology checker                          |
| [`cml-sieve`](programs/cml-sieve/README.md)           |   162 | Message-passing implementation of Sieve of Eratosthenes |
| [`count-graphs`](programs/count-graphs/README.md)     |   401 | Enumerate directed graphs                  |
| [`delta-blue`](programs/delta-blue/README.md)         |   703 | Delta-Blue incremental constraint solver **(BROKEN)** |
| [`dlx`](programs/dlx/README.md)                       | 1,887 | DLX simulator **(BROKEN)**                 |
| [`fannkuch`](programs/fannkuch/README.md)             |    72 | Array permutations                         |
| [`fft`](programs/fft/README.md)                       |   166 | Fast-Fourier-transform implementation      |
| [`id-ray`](programs/id-ray/README.md)                 |   xxx | A ray tracer ported from Id                |
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
| [`plclub-ray`](programs/plclub-ray/README.md)         | 2,026 | A raytracer from the ICFP'20 Programming Contest |
| [`ratio-regions`](programs/ratio-regions/README.md)   |   486 | Image segmentation/contour finding         |
| [`ray`](programs/ray/README.md)                       |   352 | A minimal ray tracer                       |
| [`regex`](programs/regex/README.md)                   |   xxx | **(BROKEN)**                               |
| [`sat`](programs/sat/README.md)                       |    54 | Brute-force SAT solver                     |
| [`simple`](programs/simple/README.md)                 |   769 | A spherical fluid-dynamics program         |
| [`smith-nf`](programs/smith-nf/README.md)             |   372 | Computes the Smith Normal Form             |
| [`stream-sieve`](programs/sieve/README.md)            |    58 | Functional stream implementation of Sieve of Eratosthenes |
| [`tsp`](programs/tsp/README.md)                       |   303 | A Traveling-Sales-Person solver            |
| [`twenty-four`](programs/twenty-four/README.md)       |   120 | CPS-style solver for the 24 puzzle         |
| [`tyan`](programs/tyan/README.md)                     |   500 | Grobner Basis calculation                  |
| [`vliw`](programs/vliw/README.md)                     | 2,940 | A Very-Long-Instruction-Word instruction scheduler |

## Scripts

In addition to the `run.sh` script, there are a couple of other utility scripts in
the `bin` directory.

### `cloc.sh`

The `cloc.sh` script can be used to count the number of source lines in
the benchmark programs.  It uses the [`cloc`](https://github.com/AlDanial/cloc)
program and reports the number of blank, comment, and code lines.

### `make-single-file.sh`

The `make-single-file.sh` script is used to create a single compilation unit for the
benchmark sources.  It is used by the `cloc.sh` and `run.sh`

