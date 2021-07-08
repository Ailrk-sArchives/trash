-- <Highest and Lowest>
module Kyu7.HighestAndLowest where
import Test.Hspec
import Test.QuickCheck


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

-- testing --------------------------------------------------------------------
spec :: Spec
spec =
  it "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" $
    highAndLow "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" `shouldBe` "542 -214"
