### Shortest path
optimal sub structure.

### Weighted graph:
G(V, E, w) where w is the weight function.

δ(s, v): shortest path from source s to v.
d(v): current weight.
π(v): predecessor on best path.

goal: get d(v) = δ

### Generic algorithm for shortest path algorithm.
-- with no negative cycles.
initiate for v in V, d[v] <- inf, pred(v) = nil, d[s] = 0.

repeat select edges [somehow]
relax edge (u, v, w)
until you can't relax anymore.

### Relaxiation (Tool 1)
if d[v] > d[u] + w(w, v)
  d[v] = d[u] + w(w, v)
  π(v) = u

### Optimal substructure (Tool 2)
Subpaths of a shortest path are shortest path


### problems for negative weights:

1. negative cycle. Doesn't terminate, goes to negative infinite.
