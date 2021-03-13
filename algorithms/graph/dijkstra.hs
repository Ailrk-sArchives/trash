{-# LANGUAGE FlexibleInstances #-}

module Dijkstra where

import           Data.Foldable
import           Data.List

-- little priority queue.
-- we maintain a mean heap of DistanceEntry: each nodes and their distance
-- to starting node.
data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)

instance Functor SkewHeap where
  fmap f Empty              = Empty
  fmap f (SkewNode x h1 h2) = SkewNode (f x) (fmap f h1) (fmap f h2)

instance Applicative SkewHeap where
  pure x = SkewNode x Empty Empty
  (<*>) = error "no"

instance (Ord a) => Semigroup(SkewHeap a) where
  heap1@(SkewNode x1 l1 r1) <> heap2@(SkewNode x2 l2 r2)
    | x1 <= x2 = SkewNode x1 (heap2 <> r1) l1
    | otherwise = SkewNode x2 (heap1 <> r2) l2
  Empty <> heap = heap
  heap <> Empty = heap

instance (Ord a) => Monoid (SkewHeap a) where
  mempty = Empty

-- implement
instance Foldable SkewHeap where
  foldr _ b Empty            = b
  foldr f b (SkewNode x l r) = f x (foldr f (foldr f b l) r)

-- instance Traversable SkewHeap where
instance Traversable SkewHeap where
  traverse f = undefined

-- a -> f b -> t a -> f (t b)
-- traverse f (x:xs) = (fmap (f a : ) traverse f xs)

extractMin :: (Ord a) => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty            = Nothing
extractMin (SkewNode x l r) = Just (x, l <> r)

isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

newtype Vertex = Vertex String deriving (Show, Eq)

instance Ord Vertex where
  compare _ _ = EQ

type Neighbours = (Vertex, [(Vertex, Weight)])

type Weight = Int
type Graph = [Neighbours]

data DistanceEntry = DistanceEntry
  { vertex   :: Vertex
  , distance :: Int   -- distance from the source vertex.
  , prev     :: Maybe Vertex
  }
  deriving (Show, Eq)

instance Ord DistanceEntry where
  compare a b = compare (vertex a) (vertex b)

type DistanceTable = [DistanceEntry]

--  A-6--B
--  |   /| \ 5
--  1  2 |  C
--  | /  | / 5
--  D-1- E

graph :: Graph
graph = [ (Vertex "A", [ (Vertex "B", 6)
                       , (Vertex "D", 1)])
        , (Vertex "B",  [ (Vertex "A", 6)
                        , (Vertex "D", 2)
                        , (Vertex "E", 2)
                       ])
        , (Vertex "C", [ (Vertex "B", 5)
                        , (Vertex "E", 5)
                       ])
        , (Vertex "D", [ (Vertex "A", 1)
                       , (Vertex "B", 2)
                       , (Vertex "E", 1)])
        , (Vertex "E", [ (Vertex "D", 1)
                       , (Vertex "B", 2)
                       , (Vertex "C", 5)])
        ]

initTable :: Vertex -> Graph -> DistanceTable
initTable (Vertex s) = map (\(v@(Vertex lbl), _) ->
  DistanceEntry { vertex = v
                , distance = if lbl == s then 0 else maxBound :: Int
                , prev = Nothing
                })

tb = initTable (Vertex "A") graph

-- don't worry about performance. the dompiler handles it
update :: (Eq key) => (key, value) -> [(key, value)] -> [(key, value)]
update (k, v) xs = (k, v) : filter (\(k', _) -> k' /= k) xs

relax :: Vertex -> Vertex -> Vertex
relax = undefined

-- { v, d, p }

-- tace back to starting node
traceBack :: Vertex -> Vertex -> DistanceTable
traceBack = undefined

-- do we need to rebuild a min heap everytime?
-- no? if so why not just sort.

-- ok first we add all nodes to the queue.
-- at the beginning s will be at the top. with d(s) = 0;
-- then we relax each neigbour of the min, update the queue for any changes
--
-- how to update? first remove old nodes from the queue, maintain the order, and
-- add a new node simply with <>.
--
-- recurse until the queue is empty.

dijkstra :: Vertex -> Graph -> DistanceTable
dijkstra v graph = search mempty queue
  where
    queue = foldl' (<>) Empty (fmap pure (initTable v graph))

    search :: DistanceTable -> SkewHeap DistanceEntry -> DistanceTable
    search table queue | queue == Empty = table
      | otherwise = undefined
