#include <stdio.h>
#include <inttypes.h>

#ifndef NSTEPS
#define NSTEPS 5000000000ll
#endif

double run (int64_t nSteps)
{
    double acc = 0.0;
    double n = 1.0;

    for (int64_t i = 0;  i < nSteps;  ++i) {
        acc = acc + (1.0 / n) - (1.0 / (n + 2.0));
	n = n + 4.0;
    }

    return 4.0 * acc;
}

int main ()
{
    printf ("This should be an approximation of pi: %13.11f (%" PRId64 " steps)\n", run (NSTEPS), NSTEPS);
}

