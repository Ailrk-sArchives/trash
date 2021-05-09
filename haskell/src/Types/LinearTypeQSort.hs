{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types.LinearTypeQSort where

import GHC.Stack

import qualified Data.Array.Mutable.Linear as Array
import Data.Array.Mutable.Linear (Array)
import Data.Unrestricted.Linear
import Prelude.Linear hiding (partition)


quicksort :: Array Int %1-> Array Int
quicksort arr = Array.size arr &
  \(Ur len, arr') -> go 0 (len - 1) arr'

go :: Int -> Int -> Array Int %1-> Array Int
go lo hi arr = case lo >= hi of
                 True -> arr
                 False -> Array.read arr lo &
                   \(Ur pivot, arr0) -> partition arr0 pivot lo hi &
                     \(arr1, Ur ix) -> swap arr1 lo ix &
                       \arr2 -> go lo ix arr2 &
                         \arr3 -> go (ix + 1) hi arr3

partition :: Array Int %1-> Int -> Int -> Int -> (Array Int, Ur Int)
partition arr pivot lx rx
  | (rx < lx) = (arr, Ur (lx - 1))
  | otherwise = Array.read arr rx &
    \(Ur lVal, arr0) -> Array.read arr0 rx &
      \(Ur rVal, arr1) -> case (lVal <= pivot, pivot < rVal) of
                            (True, True) -> partition arr1 pivot (lx + 1) (rx + 1)
                            (True, False) -> partition arr1 pivot (lx + 1) rx
                            (False, True) -> partition arr1 pivot lx (rx + 1)
                            (False, False) -> swap arr1 lx rx &
                              \arr2 -> partition arr2 pivot (lx + 1) (rx - 1)

swap :: HasCallStack => Array Int %1-> Int -> Int -> Array Int
swap arr i j = Array.read arr i &
  \(Ur ival, arr1) -> Array.read arr1 j &
    \(Ur jval, arr2) -> (Array.set i jval . Array.set j ival) arr2

-- test


