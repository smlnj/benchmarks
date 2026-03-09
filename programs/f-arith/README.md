# F Arithmetic Benchmark

This is a small, floating-point-intensive program for computing an approximation
to pi using the arc-tangent expansion (Maclaurin series).  Specifically,\
$$
    pi = 4 arctan(1)
$$
and the Maclaurin series is\
$$
  \begin{array}{rcl}
    arctan(1) & = & 1 - \frac{1}/{3} + \frac{1}/{5} - \frac{1}{7} + \cdots \\
              & = & \sum_n{\frac{(-1)^n}{(2n + 1)}}
$$

The original benchmark is from the [TILT benchmark
suite](https://github.com/RobertHarper/TILT-Compiler).

