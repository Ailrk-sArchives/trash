module Catalan where

-- https://www.youtube.com/watch?v=GlI17WaMrtw
-- https://en.wikipedia.org/wiki/Catalan_number

-- catlan number
--
-- n: 0   1   2   3   4   ...
-- C: 1   1   2   5   14
--
-- Definition:
--           1     (2n)     (2n)!
--    Cₙ = ------- (  )  = -------   for n >= 0.
--          n + 1  ( n)    (n+1)!n!
--
-- Propety:
--         (2n)   (2n )       1    (2n)
--    Cₙ = (  ) - (   ) =  ------- (  )
--         ( n)   (n+1)     n + 1  ( n)
--
-- One application is to count the number of possible evaluation order of
-- associative binary operations, e.g how many ways to parenthesis a chain of
-- binops.

factCPS :: Int -> (Int -> Int) -> Int
factCPS n k
  | n == 1 = k 1
  | otherwise = factCPS (n - 1) (\x -> k (x * n))

factorial :: Int -> Int
factorial = flip factCPS id

-- we can prove that a catalan number is always an integer.

catalan :: Int -> Int
catalan n = fromIntegral divident `div` fromIntegral divisor
  where
    divident = factorial (2 * n)
    divisor = factorial (n + 1) * factorial n

catalanNumbers = [catalan n | n <- [1 .. 10]]
