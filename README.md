# SML/NJ Benchmark Suite

This repository contains benchmark programs and supporting script
for measuring the performance of the SML/NJ system.  These include
the "classic" benchmarks that have frequently been used to evaluate
the performance of SML/NJ (and other SML systems) in the published
literature.  The original version of this suite was assembled by
Lal George at Bell Laboratories in the late 1990s.

## Running the Benchmarks

Each directory contains a script called runit. To invoke it, the shell
variable SML must be set to the path for the SML/NJ compiler. 'runit'
executed at the top level will create a file called LOG that contains
the results of compiling and running each benchmark.

Executing

> bin/runit.sh -help

will show command line options.

## Benchmark Structure

Each benchmark directory has a file `main.sml` that defines

```sml
structure Main : BMARK = ...
```

It also contains a `sources.cm` file for compiling the benchmark.
For benchmarks that have more than one source file, the list of source
files (other than `main.sml`) can be found in `FILES`.
The `bin/make-all.sh` script uses this list to construct a
single-file version of the program.

## The Benchmark Programs

The following is a brief description of each benchmark program.
The programs are listed by directory name in alphabetical order.

### `barnes-hut`

This program is a port of the "Barnes-Hut" N-Body code.  It is based on
the third release of the C version written by Joshua E. Barnes.

### `boyer`

### `dlx`

This program is a partial implementation of the RISC instruction
set described in Patterson's and Hennessy's _Computer Architecture_.
The benchmark is taken from the [MLton](http://mlton.org) sources,
where it is in the file `DLXSimulator.sml`.  The original version
was written by Matthew Fluet.

### `fft`

### `id-ray`
This is a port of a ray tracer written in the Id programming language
as part of the Impala benchmark suite.

### `knuth-bendix`
An implementation of the Knuth-Bendix completion algorithm written by
Gerard Heut (in CAML).  The input is some axioms of geometry.

### `lexgen`
A lexical-analyzer generator, implemented by James Mattson and David Tarditi
at Princeton.  The input is the lexical specification for Standard ML.

This program was one of the original **SML/NJ** benchmarks described in
Appel's *Compiling with Continuations*.

### `life`
Conway's game of life from Chris Reade's book
> *Elements of Functional Programming* <br/>
> Addison-Wesley, 1989

This program was one of the original **SML/NJ** benchmarks described in
Appel's *Compiling with Continuations*.

### `logic`

### `mandelbrot`

A simple program that computes the [Mandelbrot](https://en.wikipedia.org/wiki/Mandelbrot_set)
iteration for a 2048×2048 grid with a maximum count of 1024.  The program
was written by John Reppy and serves as a good test of compiling nested loops
wrwitten as nested tail-recursive functions.

### `mazefun`

A SML port of the [`mazefun`](http://www.larcenists.org/R7src/mazefun.scm)
rectangular-maze generator.  The original Scheme program was written by
Marc Feeley and ported to Standard ML by Kavon Farvardin as part of the
Manticore Project.  We generate a 15x15 maze 10,000 times (the original
benchmark generated a 11x11 maze).

### `mc-ray`

A SML implementation of a Monte Carlo ray tracer (aka path tracer) based
on the [*Ray Tracing in One Weekend*](https://raytracing.github.io)
books by Peter Shirley.  The SML version was written by John Reppy and
renders a 150x100 image of scene of 124 random spheres using 50 samples
per pixel.

### `mlyacc`

A LALR(1) parser generator, implemented by David Tarditi
at Princeton.  The input is the Standard ML grammar.

This program was one of the original **SML/NJ** benchmarks.

### `nucleic`

The ["Pseudoknot"](https://doi.org/10.1017/S0956796800001891) benchmark,
which is a floating-point intensive program taken from molecular
biology.  The original program was written in Scheme by Marc Feeley.

### `plclub-ray`

This is a port of the PLClub OCaml winning entry to the 2000 ICFP
programming contest.  The original port to SML was done by Stephen
Weeks for the MLton benchmarks.  In this version, the code has been
split up into separate files and some of the OCaml names have been
changed to the corresponding SML names (*e.g.*, `float` replaced by
`real`).  The test input is the chess board scene by Leif Kornstaedt
and it is rendered 10 times.


### `ray`

### `simple`

A spherical fluid-dynamics program written as a Fortran benchmark by Crowley *et al* at
Lawrence Livermore Labs in 1978.  It was first ported to Id by Ekanadham and Arvind,
and then to Standard ML by Lal George.

This program was one of the original **SML/NJ** benchmarks described in
Appel's *Compiling with Continuations*.

### smith-nf

Computes the [Smith Normal Form](https://en.wikipedia.org/wiki/Smith_normal_form)
for a 35x35 integer matrix using `IntInf.int` for the matrix elements.  The
benchmark is taken from the [MLton](http://mlton.org) sources and was written
by Henry Cejtin.

### `tsp`

This is a *travelling-sales-person* solver that was ported to Standard ML
from the original Olden benchmarks by John Reppy.

### `vliw`

A Very-Long-Instruction-Word instruction scheduler written by John Danskin.

This program was one of the original **SML/NJ** benchmarks described in
Appel's *Compiling with Continuations*.

## Scripts

### `make-all.sh`

The `make-all.sh` script is used to create a single compilation unit for the
benchmark sources.

### `runit.sh`

The `runit.sh` script is used to run one or more benchmark programs.

