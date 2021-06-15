-- <Square into Squares. Protect trees!>

module Kyu4.SquareIntoSquares where


-------------------------------------------------
-- primitive attempt.
-------------------------------------------------
-- fold is the cloest thing you can get compare to a loop with break.
-- the accumulator can also be used to control the default return value.

decompose :: Integer -> Maybe [Integer]
decompose n = decompose' n (n * n)

decompose' :: Integer -> Integer -> Maybe [Integer]
decompose' n r
  | r == 0 = return [n]
  | otherwise = foldl f Nothing [n - 1, n - 2 .. 1]
  where
    f b i =
      let x = r - i ^ 2
       in if x >= 0
            then case b of
              Nothing -> (decompose' i x)
              _       -> (++) <$> (decompose' i x) <*> b
            else Nothing

{-
It is asking about how to get { (ai)i∈z | ∑ai^2 = n^2 }, where (ai) is a
strictly increasing sequence.
This sequence can be regarded as solving a pythagorean n-tuple.
paper for solving pythagorean n-tuple: https://arxiv.org/pdf/1201.2145.pdf
-}
