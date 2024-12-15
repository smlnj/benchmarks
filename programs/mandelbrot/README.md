# The mandelbrot benchmark

This program computes the [Mandelbrot](https://en.wikipedia.org/wiki/Mandelbrot_set)
iteration for a 2048×2048 grid with a maximum count of 1024.  The
program was written by John Reppy and serves as a good test of
compiling nested loops written as nested tail-recursive functions.

The original program computed a 2D color image from the individual
iteration counts, but this benchmark just computes the total number
of iterations.
