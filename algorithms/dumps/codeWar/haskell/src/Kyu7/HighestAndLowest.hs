-- <Highest and Lowest>
module Kyu7.HighestAndLowest where

--------------------------------------------
-- first attempt
--------------------------------------------
-- (read a :: Int) to convert string into int
highAndLow :: String -> String
highAndLow str = show (foldr max (minBound :: Int) list) ++
                 " " ++ show (foldr min (maxBound :: Int) list)
  where
    split :: Eq a => a -> [a] -> [[a]]
    split d [] = []
    split d s = let (x, y) = span (/=d) s
                 in x : split d (drop 1 y)
    list = (read <$> split ' ' str) :: [Int]

--------------------------------------------
-- better version
--------------------------------------------
-- words tokenize string by space
-- max min take two args and return the bigger one.
-- maximum and minimum are max and min you want.
highAndLow' :: String -> String
highAndLow' str = show (maximum ns) ++ " " ++ show (minimum ns)
  where ns = (map read $ words str) :: [Int]
