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

Each directory contains a script called runit. To invoke it, the shell
variable SML must be set to the path for the SML/NJ compiler. 'runit'
executed at the top level will create a file called LOG that contains
the results of compiling and running each benchmark.

Executing

> bin/runit.sh -help

will show command line options.

## Benchmark Directory Structure

Each benchmark directory contains the following files (not all of which
may be present):

* `main.sml` -- a SML file that defines the `Main` structure.

* `sources.cm` -- a CM file for building the benchmark program.

* additional SML source files for the benchmark.

* `FILES` -- an optional list of additional source files (other than
  `main.sml`) in compilation order.  The `bin/make-single-file.sh` script
  uses this list to construct a single-file version of the program.
  This file is omitted for benchmarks that have `main.sml` as their
  only source file

* `ANSWER` -- an optional text file containing the correct output from
  running the `Main.testit` function.  Some benchmarks currently do not
  have test output; for these benchmarks, the `ANSWER` file is omitted.

* `DATA` -- an optional directory containing any input data files required
  by the benchmark program.

## The Benchmark Programs

The following is a brief description of each benchmark program.
The programs are listed by directory name in alphabetical order;
for more detailed information, see the `README.md` file in each
directory.

| Directory                                             | LOC   | Short Description                          |
| ----------------------------------------------------- | ----- | ------------------------------------------ |
| [`barnes-hut`](programs/barnes-hut/README.md)         |   820 | Hierarchical N-body solver                 |
| [`black-scholes`](programs/black-scholes/README.md)   |   126 | European-style option pricing              |
| [`boyer`](programs/boyer/README.md)                   |   912 |                                            |
| [`count-graphs`](programs/count-graphs/README.md)     |   399 |                                            |
| [`delta-blue`](programs/delta-blue/README.md)         |   703 | Delta-Blue incremental constraint solver   |
| [`dlx`](programs/dlx/README.md)                       | 1,887 |                                            |
| [`fft`](programs/fft/README.md)                       |   166 |                                            |
| [`id-ray`](programs/id-ray/README.md)                 |   xxx | Ray tracer ported from Id                  |
| [`knuth-bendix`](programs/knuth-bendix/README.md)     |   430 | Knuth-Bendix completion algorithm          |
| [`lexgen`](programs/lexgen/README.md)                 | 1,036 | lexical-analyzer generator                 |
| [`life`](programs/life/README.md)                     |   118 | Conway's game of life                      |
| [`logic`](programs/logic/README.md)                   |   317 | Knuth-Bendix completion algorithm          |
| [`mandelbrot`](programs/mandelbrot/README.md)         |    49 | Mandelbrot-set computation                 |
| [`mandelbrot-rat`](programs/mandelbrot-rat/README.md) |   xxx | Mandelbrot-set computation using rationals |
| [`mazefun`](programs/mazefun/README.md)               |   185 | Maze generator                             |
| [`mc-ray`](programs/mc-ray/README.md)                 |   736 | Monte-Carlo ray tracer                     |
| [`mlyacc`](programs/mlyacc/README.md)                 | 5,566 | LALR(1) parser generator                   |
| [`nbody`](programs/nbody/README.md)                   |   135 | O(n^2) N-body solver                       |
| [`nucleic`](programs/nucleic/README.md)               | 2,920 |                                            |
| [`pia`](programs/pia/README.md)                       |   xxx |                                            |
| [`pingpong`](programs/pingpong/README.md)             |   126 | Message-passing microbenchmark             |
| [`plclub-ray`](programs/plclub-ray/README.md)         | 2,026 | Another ray tracer                         |
| [`ratio-regions`](programs/ratio-regions/README.md)   |   xxx |                                            |
| [`ray`](programs/ray/README.md)                       |   352 |                                            |
| [`sieve`](programs/sieve/README.md)                   |   159 | Message-passing implementation of Sieve of Eratosthenes |
| [`simple`](programs/simple/README.md)                 |   760 | A spherical fluid-dynamics program         |
| [`smith-nf`](programs/smith-nf/README.md)             |   371 | Computes the Smith Normal Form             |
| [`tsp`](programs/tsp/README.md)                       |   304 | A Traveling-Sales-Person solver            |
| [`vliw`](programs/vliw/README.md)                     | 2,934 | A Very-Long-Instruction-Word instruction scheduler |

## Scripts

### `cloc.sh`

The `cloc.sh` script can be used to count the number of source lines in
the benchmark programs.  It uses the [`cloc`](https://github.com/AlDanial/cloc)
program and reports the number of blank, comment, and code lines.  It
applies `cloc` to the result of `make-single-file.sh`, so the result includes the
`BMARK` signature and three extra lines of code for the `local` declaration.

### `make-single-file.sh`

The `make-single-file.sh` script is used to create a single compilation unit for the
benchmark sources.

### `runit.sh`

The `runit.sh` script is used to run one or more benchmark programs.

