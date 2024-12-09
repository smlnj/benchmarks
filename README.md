# SML/NJ Benchmark Suite

This repository contains benchmark programs and supporting script
for measuring the performance of the SML/NJ system.  These include
the "classic" benchmarks that have frequently been used to evaluate
the performance of SML/NJ (and other SML systems) in the published
literature.  The original version of this suite was assembled by
Lal George at Bell Laboratories in the late 1990s.

Each directory contains a script called runit. To invoke it, the shell
variable SML must be set to the path for the SML/NJ compiler. 'runit'
executed at the top level will create a file called LOG that contains
the results of compiling and running each benchmark.

Executing

> runit -help

will show command line options.
