{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Dijkstra where

import           Control.Monad.ST
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.STRef

-- ------------------------------------------------------------------------------

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

instance Foldable SkewHeap where
  foldr _ b Empty            = b
  foldr f b (SkewNode x l r) = f x (foldr f (foldr f b l) r)

extractMin :: (Ord a) => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty            = Nothing
extractMin (SkewNode x l r) = Just (x, l <> r)

-- filter a skew heap
heapDeleteBy :: forall a. (Ord a)
             => (a -> Bool) -- whether delete the node if hit the key
             -> SkewHeap a
             -> Maybe ([a], SkewHeap a)
heapDeleteBy pred h = runST $ do
  ref <- newSTRef []
  h' <- go (\x -> modifySTRef ref (x:)) h
  acc <- readSTRef ref
  return $ Just (acc, h')
  where
    go :: (Monad m) => (a -> m ()) -> SkewHeap a -> m (SkewHeap a)
    go _ Empty              = pure Empty
    go modify t@(SkewNode x l r) =
      if pred x
         then modify x >> (<>) <$> go modify l <*> go modify r
           else do
             l' <- go modify l
             SkewNode x l' <$> go modify r


-- update a vertex that satisfied the preidcate.
heapModify :: Ord a => (a -> Bool) -> (a -> a) -> SkewHeap a -> SkewHeap a
heapModify pred f h = case heapDeleteBy pred h of
                        Nothing       -> h
                        Just (xs, h') -> mconcat (fmap (pure . f) xs) <> h'

-- ------------------------------------------------------------------------------

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
  compare a b = compare (distance a) (distance b)

type DistanceTable = [DistanceEntry]

showDistanceTable = foldr (\e b -> show e ++ "\n" ++ b) ""

initTable :: Vertex -> Graph -> DistanceTable
initTable (Vertex s) = map (\(v@(Vertex lbl), _) ->
  DistanceEntry { vertex = v
                , distance = if lbl == s then 0 else maxBound :: Int
                , prev = Nothing
                })

-- ------------------------------------------------------------------------------

dijkstra :: Vertex -> Graph -> DistanceTable
dijkstra v graph = search [] queue
  where
    queue = foldl' (<>) Empty (fmap pure (initTable v graph))
    search :: DistanceTable -> SkewHeap DistanceEntry -> DistanceTable
    search table queue | queue == Empty = table
      | otherwise = fromJust $ do
        (v, queue') <- extractMin queue
        adjs <- lookup (vertex v) graph
        let dv = distance v
            modify h uk uv e =
              if dv + uv < distance e
                 then e { distance = dv + uv, prev = Just (vertex v) }
                 else e
            f h (uk, uv) =
              heapModify ((uk ==) . vertex) (modify h uk uv) h
            queue'' = foldl' f queue' adjs
        return (search (v : table) queue'')

-- ------------------------------------------------------------------------------

#define TEST
#ifdef TEST

-- test input
-- A E D C B
testHeadModify :: IO ()
testHeadModify = do
  let h1 = heapModify (pred "B") (\e -> e { distance = 10 }) queue
      h2 = heapModify (pred "C") (\e -> e { distance = 8 }) h1
      h3 = heapModify (pred "D") (\e -> e { distance = 3 }) h2
      h4 = heapModify (pred "E") (\e -> e { distance = 2 }) h3
  print h4
  let Just (a, xs) = extractMin h4
  print a
  let Just (a, xs') = extractMin xs
  print a
  let Just (a, xs) = extractMin xs'
  print a
  let Just (a, xs') = extractMin xs
  print a
  let Just (a, xs) = extractMin xs'
  print a
 where
   pred l = (\(Vertex v) -> v == l) . vertex
   v = Vertex "A"
   queue = foldl' (<>) Empty (fmap pure (initTable v graph))

-- tester
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

testDijkstra :: IO ()
testDijkstra = do
  let s = Vertex "A"
      q = dijkstra s graph
  putStr (showDistanceTable q)

-- -- output
-- DistanceEntry {vertex = Vertex "C", distance = 7, prev = Just (Vertex "E")}
-- DistanceEntry {vertex = Vertex "B", distance = 3, prev = Just (Vertex "D")}
-- DistanceEntry {vertex = Vertex "E", distance = 2, prev = Just (Vertex "D")}
-- DistanceEntry {vertex = Vertex "D", distance = 1, prev = Just (Vertex "A")}
-- DistanceEntry {vertex = Vertex "A", distance = 0, prev = Nothing}

#endif
