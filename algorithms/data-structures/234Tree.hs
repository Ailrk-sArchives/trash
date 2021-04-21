module M234Tree where

-- one step to red black tree
-- https://www.cs.purdue.edu/homes/ayg/CS251/slides/chap13b.pdf

data Tree234
  = Two Int Tree234 Tree234
  | Three Int Int Tree234 Tree234 Tree234
  | Four Int Int Int Tree234 Tree234 Tree234 Tree234
  deriving (Show, Eq)

