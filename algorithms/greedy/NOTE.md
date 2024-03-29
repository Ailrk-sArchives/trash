# Greedy method

- greedy method is used to solve optimization problem

Given a problem, there can be many solutions. But if we imporse some constraints, there will be some solutions that are feasible, and some that are not.

- feasible solution: solutions that satisfy some constraint.

- optimal solution: feasible solution that is optmized.

```
Note: other methods to solve the optimization problems:
  - Dynamic programming. Say break a big problem into several smaller
    ones, and construct the best solution for the big problem by using
    the optimal small solutions.

  - Branch and Bound:
```

## General approach

Look into all inputs, and check if it's feasible. if it is, add it into the solution.

```
n = 5
a = { a1, a2, a3, a4, a5 }
Greedy (a, n)
{
  for i = 1 to n do
    x = select (a)
    if feasible(x) then
      solution = solution + x;
}
```


#### Elements of greedy strategy
- Formulate the optimization problem into a form that we can solve it by making choice from sub optimal solutions.
- Prove there is always an optimal solution that makes the greedy choice, so we can safely make a greedy choice.
- Inductively prove making greedy choice for each step yields the optimal solution.


#### Key ingredients
- Greedy choice property
  - local optmial solutions can lead to global optmial solution.
- Optimal substructure
  -
