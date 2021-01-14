module Fingertree where

-- probably the most widely used functional data structure.

data Digit a = One a | Two a a | Tree a a a | Four a a a a
data Node a = Node2 a a | Node3 a a a
data FingerTree a = Empty
                  | Single a
                  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
