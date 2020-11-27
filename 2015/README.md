# Advent of Code in Haskell 2015

## Prequsites

- Stack
- Haskell
- Christmas 2015

## Building

`stack build`

## Running a day

    cd <to correct directory in src>

Then:

    stack runghc Day01.hs < input.txt
or

    echo -n "yourinput" | stack runghc Day01.hs

## Running a test

The tests are from the example of the day

    stack runghc test.hs
