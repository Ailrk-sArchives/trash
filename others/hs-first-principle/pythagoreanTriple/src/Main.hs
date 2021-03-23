module Main where

import Data.Int
import Control.Applicative

type PyTrip = (Int, Int, Int)

testSet :: Int -> [PyTrip]
-- testSet n = (,,) <$> [1..n] <*> [1..n] <*> [1..n]
testSet n = liftA3 (,,) [1..n] [1..n] [1..n]

isPythagoreanTriple :: PyTrip -> Bool
isPythagoreanTriple (a, b, c) = a ^ 2 + b ^ 2 == c ^ 2

checkPyTrip :: [PyTrip] -> [PyTrip]
checkPyTrip [] = []
checkPyTrip (x:xs) = if isPythagoreanTriple x
                        then x : checkPyTrip xs
                     else checkPyTrip xs

perimeter :: (Floating a) => a -> a -> a -> a
perimeter a b c = sum [a, b, c]

tripleArea :: (Floating a, Ord a) => a -> a -> a -> Maybe a
tripleArea a b c =
  if abs(a + b) > c && abs(b + c) > a
     then let p = perimeter a b c / 2
           in Just $ sqrt(product [p, p - a, p - b, p - c])
  else Nothing

main :: IO ()
main = do
  print $ tripleArea 10 20 30
  print $ tripleArea 10 20 20
  print $ checkPyTrip (testSet 10)




