# zebra Benchmark

This benchmark solves the following "zebra" puzzle, and prints the solution.
It was written by Stephen Weeks (sweeks@acm.org) in 1999.  The discussion below
is from his source code.

There are 120^5 ~= 24 billion possibilities, so exhaustive search should
work fine, but I decided to write something that was a bit more clever.
It took me longer to write (2.5 hours) than to write exhaustive search, but
it runs fast (0.06 seconds on my 400MhZ P6). The code only needs to explore
3342 posibilites to solve the puzzle.

Here is the puzzle.

This word problem has 25 variables and 24 are given values. You must
solve the 25th.

The trick is HOW?

If you look at the problem mathematically, no sweat. If you get lost
in the English, you are dead.

You will know you are right by checking the answer with all the conditions.

Less than 1 percent of the population can solve this problem.

The question is: Based on the following clues, who owns the zebra?

* There are five houses.

* Each house has its own unique color.

* All house owners are of different nationalities.

* They all have different pets.

* They all drink different drinks.

* They all smoke different cigarettes.

* The Englishman lives in the red house.

* The Swede has a dog.

* The Dane drinks tea.

* The green house is adjacent to the white house on the left.

* In the green house they drink coffee.

* The man who smokes Pall Malls has birds.

* In the yellow house they smoke Dunhills.

* In the middle house they drink milk.

* The Norwegian lives in the first house.

* The man who smokes Blends lives in a house next to the house with cats.

* In a house next to the house where they have a horse, they smoke Dunhills.

* The man who smokes Blue Masters drinks beer.

* The German smokes Princes.

* The Norwegian lives next to the blue house.

* They drink water in a house next to the house where they smoke Blends.

Who owns the zebra?
