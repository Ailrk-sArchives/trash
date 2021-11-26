{-# LANGUAGE RankNTypes #-}
module Cat.Cat.Monads1 where

import           Data.Function
import           Data.Maybe

import Control.Monad.Fix
import Data.IORef
import Data.Foldable

-- imperative cyclic linked list
data Node1 = Node1 Int (IORef Node1)

-- a node points to itself.
mknode1 = mfix $ \p -> do
  p' <- newIORef (Node1 0 p)
  putStrLn "node created"
  return p'

test1 = do
  p <- mknode1
  Node1 x q <- readIORef p
  print x
  Node1 y _ <- readIORef q    -- this will explode
  print y


-- nodes point to each other.

mknode2 = mfix $ \ ~(p, r) -> do
  p' <- newIORef (Node1 0 r)
  r' <- newIORef (Node1 1 p')
  putStrLn "nodes created"
  return (p', r')

test2 = do
  (p, r) <- mknode2
  Node1 x q <- readIORef p
  print x
  Node1 y _ <- readIORef r
  print y


-- make a graph
--      0 -> 1 -> 4 -> 5
--           |
--           V
--           3 -> 2

data Node2 = Node2 Int ([IORef Node2])

mknodegraph = mfix $ \ ~(n0, n1, n2, n3, n4, n5) -> do
  n0' <- newIORef (Node2 0 [n1])
  n1' <- newIORef (Node2 1 [n3, n4])
  n3' <- newIORef (Node2 3 [n2])
  n2' <- newIORef (Node2 2 [])
  n4' <- newIORef (Node2 4 [n5])
  n5' <- newIORef (Node2 5 [])
  putStrLn "graph created"
  return (n0', n1', n2', n3', n4', n5')

test3 = do
  (n0, _, _, _, _, _) <- mknodegraph
  n <- readIORef n0
  visit n
  where
    visit :: Node2 -> IO ()
    visit (Node2 x next) = do
      print x
      m <- traverse readIORef next
      traverse_ visit m

-- result : 0 1 3 2 4 5
