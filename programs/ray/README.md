# Ray Benchmark

This is an early ray tracer benchmark that was a port of a simple C
program.  The original C program was written by Don Mitchell (Bell Labs)
and was ported to SML by John Reppy. The benchmark was used as a
standard program in the SML/NJ Benchmark suite.

This version of the benchmark has signatures for some of the modules
and produces a [PPM file](https://netpbm.sourceforge.net/doc/ppm.html)
as output.

The camera is fixed at <0,0,-3> with a 2x2 view rectangle located at
the origin.  There is a single point light located at <10, -10, -10>.
The actual ray tracer seems incorrect (e.g., the ray direction vectors
are not normalized to unit vectors), but the original C code is unavailable
to check against.  Therefore, we have left the code as is.
