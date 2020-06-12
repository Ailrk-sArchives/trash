module Data.Pictures where

import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math as Math
import Partial.Unsafe (unsafePartial)


gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
  then gcd (n - m) m
  else gcd n (m - n)

-- cool pure script feature. use the type definition directly to define func.
showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

-- pattern match failures and partial functions
-- this will fail at run time.
-- parital function. you want to avoid.
partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true

data Point = Point
  { x :: Number
  , y :: Number
  }

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

newtype Pixels = Pixels Number
newtype Inches = Inches Number
