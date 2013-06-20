A genetic algorithm for generating Brainf*ck programs in Haskell, based on the algorithm from [this article](http://www.theprojectspot.com/tutorial-post/creating-a-genetic-algorithm-for-beginners/3).

The interpreter has three stages:

- String to intermediate representation (IR)
- IR to an instruction set (ISC)
- ISC to an output string

While converting IR to ISC, it also reduces instructions, going from, say, `[Plus, Plus, Plus, Minus]` to `[Modify 2]` (3 `Plus`es + 1 `Minus` = 2 overall). Loops (not implemented in the GA yet) are also parsed into a `Loop [ISC]` form for easy manipulation.

Usage
======

Compile `main.hs` with `ghc main.hs -O3 -o ga.exe` then run `ga.exe` if you desire. You may also interpret it (slowly) with `runhaskell main.hs`.
