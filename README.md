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

## The Benchmark Programs

The following is a brief description of each benchmark program.
The programs are listed by directory name in alphabetical order.

### `barnes-hut`

This program is a port of the "Barnes-Hut" N-Body code.  It is based on
the third release of the C version written by Joshua E. Barnes.

### `boyer`

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

### `mazefun`

A SML port of the [`mazefun`](http://www.larcenists.org/R7src/mazefun.scm)
rectangular-maze generator.  The original Scheme program was written by
Marc Feeley and ported to Standard ML by Kavon Farvardin as part of the
Manticore Project.  We generate a 15x15 maze 10,000 times (the original
benchmark generated a 11x11 maze).

### `mlyacc`

A LALR(1) parser generator, implemented by David Tarditi
at Princeton.  The input is the Standard ML grammar

This program was one of the original **SML/NJ** benchmarks.

### `nucleic`

### `ray`

### `simple`
A spherical fluid-dynamics program written as a Fortran benchmark by Crowley *et al* at
Lawrence Livermore Labs in 1978.  It was first ported to Id by Ekanadham and Arvind,
and then to Standard ML by Lal George.

This program was one of the original **SML/NJ** benchmarks described in
Appel's *Compiling with Continuations*.

### `tsp`

### `vliw`

A Very-Long-Instruction-Word instruction scheduler written by John Danskin.

This program was one of the original **SML/NJ** benchmarks described in
Appel's *Compiling with Continuations*.
