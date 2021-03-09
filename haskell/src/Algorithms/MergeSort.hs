module Algorithms.MergeSort where

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort as) (mergeSort bs)
  where (as, bs) = splitHalf xs

splitHalf :: [a] -> ([a], [a])
splitHalf [] = ([], [])
splitHalf [x] = ([x], [])
splitHalf (x:y:xys) = (x:xs, y:ys)
  where (xs, ys) = splitHalf xys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y
                         then x : merge xs (y:ys)
                         else y : merge ys (x:xs)
