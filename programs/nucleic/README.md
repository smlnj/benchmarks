# Nucleic

This benchmark is the "["Pseudoknot"](https://doi.org/10.1017/S0956796800001891)"
program that was used to compare
functional-language implementations.  It does some calculations
relating to the 3D structure of nucleic acids.  The original benchmark
was a port of a Scheme program written by Marc Feeley and was done
by committee at the **Dagstuhl Workshop on Applications of Functional
Programming in the Real World** (May 1994).

This version of the benchmark has one small change: the `math_atan2`
function was replaced by `Math.atan2` (the latter was not available
when the original program was written).

## References

> Functional Programming in the Real World
> R. Giegerich and R. J. M. Hughes.
> Dagstuhl Seminar Report 89, IBFI GmbH, Schloss Dagstuhl, D-66687
> Wadern, Germany, May 1994.

> [Benchmarking Implementations of Functional Languages with ‘Pseudoknot’](https://doi.org/10.1017/S0956796800001891)
> Hartel, Pieter H. *et al.*
> Journal of Functional Programming, Volume 6, Number 4
> July, 1996
