# calculator-calculator
Solves puzzles from the game "Calculator: The Game".

## How to use

This project was built with cabal.  On the off chance you have cabal installed, and it happens to work,
use `cabal run` in the command line and follow the instructions.  Else, search the build directories in `dist-newstyle` for the executable.

## How to input available moves?

This is the syntax for describing available buttons to the solver.  It is a comma separated list of strings according to this legend.

"move[args...],move[args...],..."

- an : Add n to the current number.
- sn : Subtract n from the current number.
- mn : Multiply n with the current number.
- dn : Divide n with the current number.  Will not execute if the division produces a remainder.
- en : Exponentiate the current number to the power of n.
- f  : Flip the sign.
- \+  : Sum up the digits.
- r  : Reverse the digits.
- R  : CHOICE: round to a certain place.
- b  : Backspace the least significant digit of the current number.  Produces 0 from single digit numbers.
- B  : CHOICE: delete a certain digit.
- ^n : Change the buttons on the calculator by adding n. (n can be negative)
- |  : Mirror the digits.
- cn : Concatenates n to the current number.
- Cn : CHOICE: Insert n at any part of the number.
- /n : Filter (CUT) n from the number.
- \#  : Store the current number.
- @n : Concatenate n to the current number *from a memory cell.*
- i  : Invert all digits - subtract them from 10.
- hd : Shift to direction d, where d is either 'l' or 'r'.
- =d : Sort to direction d.
- ta>b : Transforms all occurences of the digit string a into the digit string b.

For describing calculator properties (currently only portals):

- pa>b : Describes a portal from index a to index b of the current number.  The indices run from right to left, and start from 0.

For example, with the moves "Add 1", "Reverse", "Divide by 2", and "Concatenate 4", we'd have the string:

`a1,r,d2,c4`
