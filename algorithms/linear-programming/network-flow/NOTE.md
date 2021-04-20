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


# Ford Fulkerson method

- residual network
- augmenting path
- cuts

