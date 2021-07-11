module Sums where

import           Prelude hiding (sum)

-- sum of sequence
-- n
-- ∑ x^k
-- x=1

sum :: Foldable f => Num a => f a -> a
sum = foldr (+) 0

-- 1 + 2 + .. + n
sum1 n = (n * (n + 1)) / 2
sum2 n = (n * (n + 1) * (2 * n - 1)) / 6

-- arithmetic progression
arithProgress :: Int -> Int -> [Int]
arithProgress start step = start : arithProgress (start + step) step

sumArithProgress1 start end step = let n = (start - end) / step
                                    in (n * (start + end)) / 2
