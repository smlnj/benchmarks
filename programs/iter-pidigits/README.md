# pidigits

This benchmark computes digits of pi using an iterative approach (vs. the streams
approach in the **pidigits** benchmark).  The benchmark is from the [Benchmarks
Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/pidigits.html)
and a port of the **C** version (`other/pidigits.c`).

The algorithm used is described in Section 5 of [Unbounded Spigot Algorithms for the
Digits of Pi](https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf).
