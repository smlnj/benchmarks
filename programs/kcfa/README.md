# kCFA Benchmark

A simple implementation of Shivers' k-CFA ported from a Scheme/Racket
implementation by [Matthew Might](https://matt.might.net).  The main
difference is that we use finite sets and maps from the SML/NJ Library
instead of lists to represent some of the domains.

The port was done by John Reppy and included in the 3CPS benchmarks.

The following description is from Might's source code:

> k-CFA is a well-known hierarchy of increasingly precise
> control-flow analyses that approximate the solution to the
> control-flow problem.
>
> This program is a simple implementation of an
> abstract-interpretion-based k-CFA for continuation-
> passing-style lambda calculus (CPS).
>
> Contrary to what one might expect, it does not use
> constraint-solving.  Rather, k-CFA is implemented
> as a small-step abstract interpreter.  In fact, it looks
> suspiciously like an ordinary (if non-deterministic)
> Scheme interpreter.
>
> The analysis consists of exploring the reachable
> parts of a graph of abstract machine states.
>
> Once constructed, a composite store mapping addresses
> to values is synthesized.
>
> After that, the composite store is further summarized to
> produce a mapping from variables to simple lambda terms.
>
>
> The language over which the abstract interpreter operates is the
> continuation-passing style lambda calculus (CPS):
>
> exp  ::= (ref    <label> <var>)
>       |  (lambda <label> (<var1> ... <varN>) <call>)
> call ::= (call   <label> <exp0> <exp1> ... <expN>)
>
> label = integer
>
> CPS is a popular intermediate form for functional compilation.
> Its simplicity also means that it takes less code to construct
> the abstract interpreter.

