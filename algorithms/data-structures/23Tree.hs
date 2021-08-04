module M23Tree where

-- 2-3 tree, yet another balance tree

-- 2-3 tree. Insertion and deletion might restructure the tree to
-- keep it balance.

-- lookup: O(logn)
-- insert: O(logn)
-- delete: O(logn)

-- Some invariants:
-- 1. all leaves are at the same depth.
-- 2. information only stored at leaves.
-- 3. keys are ordered left to right.
--
-- It's essentially a small b tree.

-- intermediate 2-3 tree representation with a temporary four nodes
data Tree23' a
  = Two' a (Tree23' a) (Tree23' a)
  | Tree' a a (Tree23' a) (Tree23' a) (Tree23' a)
  | Four' a a a  (Tree23' a) (Tree23' a) (Tree23' a) (Tree23' a)
  deriving (Show, Eq)

data Tree23 a
  = Two a (Tree23 a) (Tree23 a)
  | Tree a a  (Tree23 a) (Tree23 a) (Tree23 a)

