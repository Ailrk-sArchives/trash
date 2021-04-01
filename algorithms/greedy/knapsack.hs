module Knapsack where

import Data.List
import Debug.Trace
import Text.Printf

-- fractional knapsack problem.
-- or Thief robbing store problem. In this setting you are allowed to grab
-- a fraction of the item, which makes it possible to be solved with a greedy
-- algorithm.

-- item(th)  1   2   3   4   5   6   7
-- profit    10  5  15   7   6   18  3
-- weight    2   3   5   7   1   4   1

-- the solution is super simple, just take the most valuable until there
-- are not more same item or no more weight.

data I = I Double Double deriving (Show)

capacity = 15

items = [I 10 2, I 5 3, I 15 5, I 7 7, I 6 1, I 18 4, I 3 1]

knapsack :: [I] -> Double -> Double
knapsack is weight = go byprofit 0 0
  where
    profitKey (I a _) (I b _) = compare a b
    byprofit = sortBy (flip profitKey) is
    go :: [I] -> Double -> Double -> Double
    go [] p _ = p
    go ((I p' w') : xs) p w =
      trace (printf "profit %.2f weight %.2f" p w) $ -- use trace + printf to debug.
        if w + w' > weight
          then
            let wfrac = (weight - w) / w'
                pfrac = wfrac * p'
             in pfrac + p
          else go xs (p + p') (w + w')

profit = knapsack items capacity
