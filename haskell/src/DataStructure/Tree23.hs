{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

-- https://en.wikipedia.org/wiki/2%E2%80%933_tree#
module DataStructure.Tree23
  ( Tree23(..)) where

---------------------------------------------------------- --------------------
-- 23 tree
-- It's probably the simplest form of spliting tree with nodes that have
-- different capacities. There are many other variations of the similar scheme,
-- e.g b+ tree.
--
-- 23 tree balance itself by spliting and rotation.
-- The overall tree will try to form a full tree until it can't, then it
-- will introduce a new layer at the root node by propagating the rotation all
-- the way up.

-- This implementation follows the same scheme of B+ tree, which stores all
-- elements in leaf nodes. It's proven to be much easier to manage because the
-- "tree" structure can be stored separately, while data can be easily arranged
-- in a sequential container.

-------------------------------------------------------------------------------
-- temporary representation with four node
data Tree23' k a where
  Leaf' :: k -> a -> Tree23' k a
  Two' :: Ord k => k -> !Int -> (Tree23' k a) -> (Tree23' k a) -> Tree23' k a
  Three' :: Ord k => k -> k
         -> !Int
         -> (Tree23' k a) -> (Tree23' k a) -> (Tree23' k a)
         -> Tree23' k a
  Four' :: Ord k          -- temporary node with three data elements
        => k -> k -> k
        -> a
        -> !Int
        -> (Tree23' k a) -> (Tree23' k a) -> (Tree23' k a)
        -> Tree23' k a

data Tree23 k a where
  Leaf :: k -> a -> Tree23 k a
  Two :: Ord k => k -> !Int -> Tree23 k a -> Tree23 k a -> Tree23 k a
  Three :: Ord k
        => k -> k
        -> !Int
        -> Tree23 k a -> Tree23 k a -> Tree23 k a
        -> Tree23 k a


-------------------------------------------------------------------------------
-- We can analyse the amortized complexity of splitting for 23 tree:

