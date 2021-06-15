-- <Find the odd int>
--
-- Given an array, find the int that appears an odd number of times.
-- There will always be only one integer that appears an odd number of times.

module Kyu6.FindTheOddInt (findOdd) where

import Data.Bits (xor)
import Data.List

-- 2019-11-19
--  Given a list, find the [Int] that appears an
--  odd number of times. The tests will always
--  provide such a number, and the list will
--  always contain at least one element.
-----------------------------------------------
-- first version
-----------------------------------------------
findOdd'' :: [Int] -> Int
findOdd'' = fst <$> head . filter (odd.snd) . table
  where
    f a t =
      case lookup a t of
        Nothing -> (a, 1) : t
        Just _ ->
          fmap (\(x, n) ->
            if x == a then (x, n + 1) else (x, n)) t
    table :: [Int] -> [(Int, Int)]
    table = foldr f [(0, 0)]

-----------------------------------------------
-- smart version
-----------------------------------------------

findOdd' :: [Int] -> Int
findOdd' = foldr1 xor
-- Note foldr1 has no base case
-- only apply to non empty structure.


-----------------------------------------------
-- list compherhension version
-----------------------------------------------
findOdd :: [Int] -> Int
findOdd xs = head [ x | x <- xs, odd . length . filter (==x) $ xs]

-----------------------------------------------
-- map version
-----------------------------------------------
findOddMap :: [Int] -> Int
findOddMap = head . map head . filter (odd.length) . group . sort



