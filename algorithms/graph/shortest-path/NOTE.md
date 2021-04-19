### Shortest path
optimal sub structure.

### Weighted graph:
G(V, E, w) where w is the weight function.

d(v): current weight.
π(v): predecessor on best path.

### Generic algorithm for shortest path algorithm.
initiate for v in V, d[v] <- inf, pred(v) = nil, d[s] = 0.

repeat select edges [somehow]
relax edge (u, v, w)
until you can't relax anymore.
