# Ratio-Regions Benchmark

This benchmark is from the MLton benchmarks.  It was translated from
Jeff Siskind's Scheme code by Stephen Weeks.

Here is the description from Jeff Siskind:

> It is an implementation of Ratio Regions, an image segmentation/contour finding technique due to Ingemar Cox,
> Satish Rao, and Yu Zhong. The algorithm is a reduction to max flow, an
> unpublished technique that Satish described to me. Peter Blicher originally
> implemented this via a translation to Andrew Goldberg's generic max-flow code.
> I've reimplemented it, specializing the max-flow algorithm to the particular
> graphs that are produced by Satish's reduction instead of using Andrew's code.
> The max-flow algorithm is preflow-push with periodic relabeling and a
> wave-based heuristic for scheduling pushes and lifts due to Sebastien Roy.

The MLton version has been modernized by directly using SML APIs and
organizing the code in modules.
