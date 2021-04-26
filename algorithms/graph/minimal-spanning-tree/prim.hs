module Prim where

-- prim algorithm is a greedy method to find the minial spanning tree in a graph.
--
-- Minimal spanning tree is a connected weighted undirected graph that contains
-- all vertices, and edges being the subset of the original graph, while the sum
-- of weights of edges is minimum.
--
-- collapse vertexes
-- the example graph
-- https://en.wikipedia.org/wiki/Kruskal%27s_algorithm#:~:text=Kruskal's%20algorithm%20finds%20a%20minimum,finds%20a%20minimum%20spanning%20tree.&text=It%20is%20a%20greedy%20algorithm,to%20the%20minimum%20spanning%20forest.
--
-- Process:
--  1. choose arbirary starting point as root from the graph,
--  2. grow the tree by on edge. find the minimum weight edge and transfer it to the tree.
--  3. repeat until all vertices are in the tree.


import Data.List

data Node = Node String Int deriving (Show, Eq)

type Graph = [(String, [Node])]

graph :: Graph
graph =
  [ ("A", [Node "B" 7, Node "D" 5]),
    ("B", [Node "A" 7, Node "D" 9, Node "E" 7, Node "C" 8]),
    ("C", [Node "B" 8, Node "E" 5]),
    ("D", [Node "A" 5, Node "B" 9, Node "E" 15, Node "F" 6]),
    ("E", [Node "D" 15, Node "B" 7, Node "C" 5, Node "F" 8, Node "G" 9]),
    ("G", [Node "F" 11, Node "E" 9])
  ]



-- | prim algorithm to find the minimal spanning tree.
prim :: Graph -> Graph
prim = undefined
