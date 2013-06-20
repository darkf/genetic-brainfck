A genetic algorithm for generating Brainf*ck programs in Haskell, based on the algorithm from [this article](http://www.theprojectspot.com/tutorial-post/creating-a-genetic-algorithm-for-beginners/3).

The interpreter has three stages -- string to intermediate representation (IR), IR to an instruction set (ISC), and ISC to an output string.

While converting IR to ISC, it also reduces instructions, going from, say, `[Plus, Plus, Plus, Minus]` to `[Modify 2]` (3 `Plus`es + 1 `Minus` = 2 overall). Loops (not implemented in the GA yet, but are in the interpreter) are also parsed into a `Loop [ISC]` form for easy manipulation.
