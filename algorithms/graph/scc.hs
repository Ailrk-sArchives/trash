module StronglyConnectedComponent where
-- TODO

-- A directed graph is strongly connected if every vertex is reachable
-- from other vetex.
--
-- The set of all scc parition an arbitrary directed graph.

-- Finding strongly connected components with tarjan's algorithm.


newtype Vertex = Vertex String deriving (Show, Eq)
type Neighbours = (Vertex, [(Vertex, Weight)])

type Weight = Int
type Graph = [Neighbours]

instance Ord Vertex where
  compare _ _ = EQ


