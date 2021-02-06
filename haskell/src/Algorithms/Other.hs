module Algorithms.Other where

type Set a = [a]

-- the powerset
powerset :: Set a -> Set (Set a)
powerset []     = [[]]
powerset (x:xs) = let current = [ x:ps | ps <- powerset xs]
                      rest = powerset xs
                   in current ++ rest
