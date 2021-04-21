# Flow network

A flow network G(V, E) is a directed graph with a source and a sink. We can define a capacity function for each edge (u,v) in E, which represents the maximum material that can flow through the edge. We assume there exists a path from source s to sink t, and all vertices are in the path. That is, there exists a path s ~> v ~> t for all v in G.


### Properties

define f to be the flow, we have

- Capacity constraint: f(u, v) < c(u, v)
- Skew symmetry:       f(u, v) = -f(v, u)
- Flow conservation:    for all u in V - {s, t}, we have Sum(f(u, v)) = 0

Note if we don't have flow conservation, material will accumulate, the flow is actually get blocked.


define value of f:

|f| = Sum(f(s, v))
     v in V

### Some concepts

- residuals: cf(e) = c(e) - f(e)
- augmenting path: a path in the residual network that connects s to t.
- multiple sources and sink can be connected to single mega source and sink and sovle in the same way.


### Classifying flow problems
- maximum flow problem
- multi commodity flow problem
- minimum cost flow problem
- circulation problem

### Ford Fulkerson method

input:  G = (V, E) with flow capacity c and source s, sink t.
output: compute a maximum flow f from s to t

1. f(u, v) <_ o for all edges (u, v)
2. while there is a path p from s to t in G, such that cf(u, v) > 0 for all edges (u, v) in p:
  1. find cf(p) = {cf(u, v) | (u, v) in p}
  2. for each edges (u, v) in p:
    1. f(u, v) <- f(u, v) + cf(p) // send flow along the path
    2. f(v, u) <- f(v, u) - cf(p) // flow might be returned later.

The path can be found bfs or dfs. The method that uses bfs is called edmonds karp algorithm.


### Cuts and flow.

- A cut C(S, T) is a partition of V into S and T = V - S such that s in S and t in T.

### Max flow min cut theorem
The following are equivalent
1. f is a maximum flow
2. Gf has no augmenting path
3. f = c(S, T) for some cut (S, T)

### Maximum biartit mathcing
