# Sieve Benchmark

This benchmark computes the 12000'th prime number using the
[**Sieve of Eratosthenes**](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
implemented as a pipeline of threads.  Each thread in the
pipeline filters out a prime number; when a new prime is
discovered, a new thread is created at the end of the pipeline.

The benchmark computes the 12000'th prime number.

The benchmark is based on an example in "Concurrent Programming in ML"
by John Reppy (Cambridge; 1999) and is from the Manticore benchmark
suite.
