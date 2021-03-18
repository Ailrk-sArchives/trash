module BfsDfsFGPuzzle where

-- https://www.nmattia.com/posts/2016-07-31-bfs-tree.html
-- Puzzle:
--  given number x0 and target xf, and two functions
--   f(x) = 2x + 1,
--   g(x) = 3x + 1
--  Give minimal number of applications of f and g that you need in order
--  to reach xf from x0, or state that one cannot reach from xf from x0
--  e.g
--    1. for xf = 4, x0 = 1
--       xf = 4 = 3 . 1 + 1 = g(1) = g(x0)
--    2. xf = 10, x0 = 1
--       xf = 10 = 3.3 + 1 = g(3) = g(2.1+1) = g.f(1) = g.f(x0)
--    3. it's impossible to reach xf= 8 from x0 = 1
-- How to solve?
--   You have two options each time, so essentailly you want to encode the solution
--   in a binary tree and bfs over it.


f x = 2 * x + 1
g x = 3 * x + 1

-- keep l and r lazy so we can have infinite trees. only l and r matters because
-- they are recursively defined

data Tree = T { depth :: !Integer
              , value :: !Integer
              , l     :: Tree
              , r     :: Tree       -- to have infinite trees.
              }
              deriving Show


-- generate a tree to play with

mkTree :: Integer -> Tree
mkTree = go 0
  where
    go d v = T { depth = d
               , value = v
               , l = go (d + 1) (f v)
               , r = go (d + 1) (g v)
               }


-- side note, a complete binary tree.

completeBinaryTree :: Tree
completeBinaryTree = go 0 1
  where
    go d v = T { depth = d
               , value = v
               , l = go (d + 1) (2 * v)
               , r = go (d + 1) (2 * v + 1)
               }


-- O(1) enqueue and dequeue.
-- The natural next step might be breaking out a state monad
-- and writer monad, but not necessary here.

data Queue a = Queue { front :: [a]
                     , back  :: [a]
                     }



-- tile the nodes
-- use lazyness to get priority queue ...

bfs :: Tree -> [Tree]
bfs root =
  let
    nodes = root : children
    children = concatMap (\t -> [l t, r t]) nodes
   in nodes


distance :: Integer -> Integer -> Maybe Integer
distance x0 xf = go nodes
  where
    nodes = bfs $ mkTree x0
    go (t:ts)
      | value t == xf = Just $ depth t
      | value t > 4 * xf = Nothing
      | otherwise = go ts
