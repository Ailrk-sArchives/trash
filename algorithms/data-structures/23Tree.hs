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

data Tree23
  = Two Int Int Tree23 Tree23
  | Tree Int Int Int Tree23 Tree23 Tree23
  deriving (Show, Eq)
