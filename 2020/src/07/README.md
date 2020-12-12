# Day 7

## Part 1

Any problem statements like "this leads to this, this leads to that" feel like a graph problem, and maybe the weights of the edges are the number of bags. It doesn't look like the number of bags are used in part 1 though, so can be ignored - but are bound to show up in part 2. It's then finding a node in the graph, and counting how many unique parents it has. The input is a load of regex, luckily there's been a lot of practice so far!

It is a graph - it's a (reverse) depth search from the bag up to the parents.

This needs refactoring...

## Part 2

This should be a depth search to the children, multiplying up the bag count (edge values) as we go. I can't figure out the syntax though...maybe using a graph was overkill!