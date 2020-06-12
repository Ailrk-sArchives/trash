module Data.Pictures where

import Prelude
import Data.Foldable (foldl)
import Global as Global   -- access some common javascript functions
import Math as Math       -- javascript Math module


gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
  then gcd (n - m) m
  else gcd n (m - n)
