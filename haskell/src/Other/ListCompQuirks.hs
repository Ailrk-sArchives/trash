module Other.ListCompQuirks where

import Control.Monad

-- normal version
position :: (Num t, Eq t, Enum a, Num a, Eq a) => t -> a -> [[a]]
position 0 n = [[]]
position k n = [p : ps | p <- [1 .. n], ps <- position (k - 1) n, isSafe p ps]

-- slow in interpreter. call position for each p
positionSlow :: (Num t, Eq t, Enum a, Num a, Eq a) => t -> a -> [[a]]
positionSlow 0 n = [[]]
positionSlow k n = [p : ps | ps <- positionSlow (k - 1) n, p <- [1 .. n], isSafe p ps]

-- monad version
positionM :: (Num t, Eq t, Enum a, Num a, Eq a) => t -> a -> [[a]]
positionM k n = do
  ps <- positionM (k - 1) n
  p <- [1 .. n]
  guard (isSafe p ps) >> return (p : ps)

-- compute position first.
positionSeq :: (Num t, Eq t, Enum a, Num a, Eq a) => t -> a -> [[a]]
positionSeq k n =
  let ls = positionSeq (k - 1) n
   in ls `seq` [p : ps | p <- [1 .. n], ps <- ls, isSafe p ps]

isSafe :: (Eq a, Num a, Enum a) => a -> [a] -> Bool
isSafe p ps = not (p `elem` ps || sameDiag p ps)
  where
    sameDiag p ps = any (\(dist, q) -> abs (p - q) == dist) $ zip [1 ..] ps

queue :: (Num a, Enum a, Eq a) => a -> [[a]]
queue n = position n n
