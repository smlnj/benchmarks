# Minimax Benchmark

This is a modified version of a Manticore benchmark that builds the game
tree using the Minimax algorithm (with and without a transposition table).
The main difference between this version and the original is that the
transposition table is recreated for each iteration; without this change,
the version with the transposition table does almost no work after the
first iteration.
The original program was written by Adam Shaw.
