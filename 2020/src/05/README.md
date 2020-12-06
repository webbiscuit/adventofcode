# Day 5

## Part 1

Anything which involves doubling/halving can be a good fit for bit twiddling. If we encode B and R as 1 and F and L as 0 we'll magically get to the seat numbers.

## Part 2

If we sort our list of seat ids and zip it up against a list of expected seat ids, we can compare to find the first mismatch - that is the empty seat number.
