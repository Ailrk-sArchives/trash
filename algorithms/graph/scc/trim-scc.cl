
;; remove uncessary edges for all scc.

;; pick a node and do dfs, it will gives a traversal
;; of nodes it can reach.
;; once dfs meet the root again, also add it into the traversal.
;; in the final traversal output, there will be even number of
;; root nodes, each enclose one strongly connected component.
;; pick the longest sub string that starts from a even ith root,
;; this is the maximal scc.
;; collect nodes from the graph, preserve edges that conencets
;; nodes in the scc according to the order of the traveral, and edges
;; that connects to other scc.
;; remove edges that are not following the order.



