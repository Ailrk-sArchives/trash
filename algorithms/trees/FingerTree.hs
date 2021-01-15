module Fingertree where

-- probably the most widely used functional data structure.
--
-- Checkout the Complexity:
--  because the depth of finger tree normally is small, Ologn can
-- be regarded as const time for most of the time.
--      operation                       normal        amortized
-- head :: Seq a -> a                    O(1)           O(1)
-- tail :: Seq a -> Seq a                O(logn)        O(1)
-- cons :: a -> Seq a -> Seq a           O(logn)        O(1)
-- last :: Seq a -> a                    O(1)           O(1)
-- init :: Seq a -> Seq a                O(logn)        O(1)
-- snoc :: Seq a -> a -> Seq a           O(logn)        O(1)
-- (++) :: Seq a -> Seq a -> Seq a       O(logn)        O(logn)
-- (!!) :: Seq a -> Int -> a             O(logn)        O(logn)
--
-- Ok I spent way too long to draw this, and it's not 100% correct...
-- A finger tree looks like:
-- where the head and the tail can be accessed in constant time.
-- http://www.staff.city.ac.uk/~ross/papers/FingerTree.html

data Digit a = One a | Two a a | Tree a a a | Four a a a a

data Node a = Node2 a a | Node3 a a a

data FingerTree a = Empty
                  | Unit a
                  | More (Digit a) (FingerTree (Node a)) (Digit a)

