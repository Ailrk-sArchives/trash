module BinaryTree where

import           Data.Maybe

-- a full implementation of binary tree in haskell.
-- notice there are a lot of type wrappers, this is how haskell is,
-- make most logic at the type level.

class (Ord a) => BinaryTreeNode a where
  mergeNodes :: a -> a -> a

data BinaryTree a = Leaf
                  | Branch (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord)

instance (BinaryTreeNode a, Show a) => Show (BinaryTree a) where
  show t = prettyPrintTree t 0

prettyPrintTree ::  (BinaryTreeNode a, Show a) => BinaryTree a -> Int -> String
prettyPrintTree Leaf _ = " Leaf"
prettyPrintTree (Branch left node right) spaces =
  " " ++ show node ++ "\n" ++
  indent ++ ":" ++ prettyPrintSub left ++
  indent ++ ":" ++ prettyPrintSub right
  where
    indent = fill (spaces + 2)
    prettyPrintSub sub = prettyPrintTree sub (spaces + 2) <> "\n"
    fill = flip replicate ' '

{-@ Zipper
    using zipper to change the focus of the tree.
    zipper allows you to traverse the cursor along some data structure
    in a effcient way.

    Imagine a list [1, 2, 3, 4, 5], cursor on 3
    we can view it as: [1, 2] 3 [4, 5], meaning we currently
    focus on 3
@-}
data BranchType = LeftBranch | RightBranch deriving (Show, Eq, Ord)

data TreeDirection a = TreeDirection BranchType a (BinaryTree a)
  deriving (Eq, Show, Ord)

type TreeDirections a = [TreeDirection a]

type TreeZipper a = (BinaryTree a, TreeDirections a)

data TreeBranch a = TreeBranch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord)

instance (BinaryTreeNode a, Show a) => Show (TreeBranch a) where
  show ()
