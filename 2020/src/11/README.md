# Day 11

## Part 1

We need to build a 2d grid, loop through all the chairs, check the rules and create a new state. Haskell being immutable should help here. We're using Haskell's Vector instead of List, because we'll need random access rather than popping (for efficiency).
Maintaining the grid size can be a pain, but if we use !? lookup in the vector we can return Maybe values rather than getting an error if we're checking off grid. Then we can use mapMaybe to remove those maybes and return a list of values.

## Part 2
