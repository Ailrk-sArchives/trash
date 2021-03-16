module Iterativelog where

import           Debug.Trace


-- iterative logrithm (log star)
-- goes down very fast, so the running time growth very slow.

logStar :: Double -> Double ->  Int
logStar n b = logStar' n b 0
  where
    logStar' n b count | n > 1.0 = logStar' (logBase b n) b (count + 1)
      | otherwise = count

logStar10 :: Double -> Int
logStar10 = flip logStar 10.0

logStar2 :: Double -> Int
logStar2 = flip logStar 2.0

l = logStar10 1000

l' = logStar10 (10^40)

l'' = logStar10 (10^100)
