# Regex

Regular-expression matching based on RE derivatives.  This benchmark was
inspired by a similar Scheme program by Matthew Might, but we have added
the "not" and "intersection" operators.

The test problem is matching the L2 language from

> Generating Optimal Monitors for Extended Regular Expressions
> by Koushik Sen and Grigore Rosu, 2003
> <https://www.sciencedirect.com/science/article/pii/S157106610481051X>

which is defined to be L2 = { s#w#s'$w | w ∊ {0,1}^2 and s,s' ∊ {0,1,#}* }.
