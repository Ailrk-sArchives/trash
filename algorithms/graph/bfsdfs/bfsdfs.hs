module BfsDfs where

import Debug.Trace

type Graph = [[Integer]]

graph :: Graph
graph =
  [ [1],
    [0, 2, 3],
    [1, 3, 4, 5], -- 0 - 2
    [1, 2, 5],
    [2],
    [2, 3, 6, 7], -- 3 - 5
    [5, 10],
    [5, 8, 10],
    [7, 9],
    [8], -- 6 - 9
    [6, 7, 11],
    [10, 12],
    [11, 13], -- 10 - 12
    [12, 14],
    [13, 15],
    [14] -- 13 - 15
  ]



dfs :: Graph -> Integer -> [Integer]
dfs graph root = loop [root] [root]
  where
    loop :: [Integer] -> [Integer] -> [Integer]
    loop visited [] = visited
    loop visited (x:xs) =
      let adjs = graph !! (fromIntegral x)
          unVisisted = (filter (flip notElem visited) adjs)
          visited' = visited <> unVisisted
          stack' = unVisisted <> xs
       in loop visited' stack'

run = dfs graph 5

bfs :: Graph -> Integer -> [Integer]
bfs graph root = loop [root] [root]
  where
    loop :: [Integer] -> [Integer] -> [Integer]
    loop visited [] = visited
    loop visited queue =
      let adjs = graph !! (fromIntegral . last) queue
          unVisited = (filter (flip notElem visited) adjs)
          visited' = visited <> unVisited
          queue' = take (length queue - 1) queue <> unVisited
       in loop visited' queue'

run1 = bfs graph 5
