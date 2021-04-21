module SpareCash where

-- first let's review the essense of dynamic programming.
-- dynamic programming is a technique for optimization problem.
-- You care about

-- again, you have possible currency value 1, 5, 11, you want
-- to make up to n dolloars with the least amount of cashes.

-- this time let's apply 5 easy steps for dp.
-- f(n) = min(f(n - 1), f(n - 5), f(n - 11))

spareCash :: Integer -> Integer
spareCash 0 = 0
spareCash n = minimum (case [ spareCash (n - i) | i <- filter (n>=) [1, 5, 11]] of
                         [] -> pure 0
                         xs -> xs) + 1

run = spareCash 15

-- oh that's quick, damn


-- let me try to solve it bottom up.


