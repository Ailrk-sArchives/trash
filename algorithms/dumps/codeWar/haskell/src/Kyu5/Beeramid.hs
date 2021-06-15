module Kyu5.Beeramid where

beeramid :: Double -> Double -> Int
beeramid bonus price
  | bonus < price && bonus < 0  = 0
  | otherwise = length
  $ takeWhile (<= bonus / price) (scanl (\acc y -> acc + y^2) 1 [2..])
