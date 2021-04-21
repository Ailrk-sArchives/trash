# Dynamic programming


## For optimization problems.
- * "careful brute force"
- subproblem + reuse
- + guessing and try them all...
- *subproblem dependencies should be acyclic

## Bottom up method
- is exactly the same as top down memoization version.
- topological sort of subproblems dependency DAG.

### 5 steps to DP
1. define subproblems
2. guessing (part of the solution)
3. relate subproblem solutions
4. recurse & memoization / bottom up
5. solvee the original problem

#### Elements of dynamic programming **
- Optimal sub solutions
- Overlapping sub problems.

## Good resources
https://www.radford.edu/~nokie/classes/360/

https://slidetodoc.com/chapter-7-dynamic-programming-7-1-fibonacci-sequence-2/

http://www.cs.oswego.edu/~odendahl/coursework/csc465/notes/09-c-multistage.html

Opening in existing browser session.

## Principle of Optimality **
Suppose to solve a problem we need to make a sequence of decisions D1, D2, ..., Dn. If the sequence is optimal, then the last k decisions, 1 < k < n must be optimal.

e.g for shortest path problem, if i, i1, i2, ..., j is the shortest path, then i1, i2, ...j must be the shortest path from i1 to j.

## Forward chaining and backward chaining.
- A naive recursion is an example of backward chaining. we separate a big problem into smaller one first, then collect their results. It's a top down approach. e.g f(7) can be solved by f(6), f(5) ...

- Backward approach goes in the other direction. We solve small problems incrementally until we can solve the big problem. e.g f(1), f(2) f(3) ..., in hope you can reach the goal.


