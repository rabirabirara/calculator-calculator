# calculator-solver
Solves puzzles from the game "Calculator: The Game".

## How to use

This project was built with cabal.  On the off chance you have cabal installed, and it happens to work,
use `cabal run` in the command line and follow the instructions.  Else, search the build directories in `dist-newstyle` for the executable.

## How to input available moves?

This is the syntax for describing available moves to the solver.  It is a comma separated list of moves according to this legend.

"move[args...],move[args...],..."

- an : Add n to the current number.
- sn : Subtract n from the current number.
- mn : Multiply n with the current number.
- dn : Divide n with the current number.  Will not execute if the division produces a remainder.
- en : Exponentiate the current number to the power of n.
- f    : Flip the sign of the current number.
- r    : Reverse the digits of the current number.
- b    : Backspace the least significant digit of the current number.  Produces 0 from single digit numbers.
- cn : Concatenates n to the current number.
- ta>b : Transforms all occurences of the digit string a into the digit string b.

For example, with the moves "Add 1", "Reverse", "Divide by 2", and "Concatenate 4", we'd have the string:

`a1,r,d2,c4`
