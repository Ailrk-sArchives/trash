module Algorithms.FPearlsTheSmallestFreeNumber where

import           Data.Array
import           Data.Array.ST

-- Given an unordered set X, find the smallest n that's not in X
-- Need O(N) complexity. so can't sort

vs :: [Int]
vs = [ 08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19
     , 02, 06 ]

------------------------------------------------------------------------------
-- naive version, worts case O(n^2)
minfree' :: [Int] -> Int
minfree' xs = head (filter (not . (`elem` vs)) [0..])

------------------------------------------------------------------------------
-- Array based: use boolean array
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checkList :: [Int] -> Array Int Bool
checkList xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
  where n = length xs

checLists' :: [Int] -> Array Int Bool
checLists' xs = runSTArray $ do
  { a <- newArray (0, n) False
  ; sequence [writeArray a x True | x <- xs, x <= n]
  ; return a
  }
  where
    n = length xs

------------------------------------------------------------------------------
-- Divide and Conquer
