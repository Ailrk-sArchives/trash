
module Maze where

import           Data.Maybe (fromJust)
-- https://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs/
-- Not like tree and list, graph is not a inductive type.
-- It's defined as G(V, E) a big set of stuffs.
-- We can't very nicely represent it with algebraic data type directly.
-- How do you represent a graph in a pure language?

{-@ tying the knot
    The first big problem of representing graph is how do you
    represent cyclic reference.
    A technic to represent cyclic data structure with lazyniess.
    We use the simplest circular graph: doubly linked list as
    an example.
 @-}

-- first some demonstration of tying the know
-- x needs y needs x. perfectly ok because they are evaluated
-- only when needed.
cyclic = let x = 0 : y
             y = 1 : x
          in x

cyclic3 = let x = 0 : y <> z
              y = 1 : z <> x
              z = 3 : x <> y
           in x

data DList a = DLNode (DList a) a (DList a)

mkDList :: [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in  first
  where go :: DList a -> [a] -> DList a -> (DList a, DList a)
        go prev []     next = (next,prev)
        -- tight the know here
        go prev (x:xs) next = let this        = DLNode prev x rest
                                  (rest,last) = go this xs next
                              in  (this,last)

takeF :: Integer -> DList a -> [a]
takeF 0 _                 = []
takeF n (DLNode _ x next) = x : (takeF (n-1) next)

takeR :: Integer -> DList a -> [a]
takeR 0 _                 = []
takeR n (DLNode prev x _) = x : (takeR (n-1) prev)

--         2<.
--         |  \..
--         V     \
--         1 <--> 4
--         |
--         V
--         3

{-@ inductive graph @-}
-- we can decompose a graph into it's node, it's context (it's in edge and
-- out edge), and the rest of the graph.
data Context a = Context [a] a [a]
data Graph a = Empty
             | (Context a) :& Graph a

-- to represent the context of 1 for the directed graph above.
context1 = Context [2, 4] 1 [3, 4]

isEmpty :: Graph a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- to pattern match a graph we need to decompose a graph into one
-- context and the rest of the graph, so we can work on one node at a
-- time.
matchAny :: Graph a -> (Context a, Graph a)
matchAny = undefined

-- pattern match on a node.
ghead :: Graph a -> a
ghead g | isEmpty g = error "empty graph"
ghead g = case matchAny g of
            ((Context _ node _), _) -> node


-- our goal is to patter match on the graph. But we cannot directly do this
-- because there is no one good starting point, since every nodes can be the
-- root node.

main :: IO ()
main = undefined
