module Kyu6.FindTheOddIntSpec where

import Data.List (permutations)
import Data.Foldable (for_)
import Kyu6.FindTheOddInt
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf (printf)

spec :: Spec
spec = do
  prop "works for singleton lists" $ \x ->
    findOdd [x] `shouldBe` x
  prop "works for lists with three elements" $ \x y ->
    x /= y ==> do
      findOdd [x, y, y] `shouldBe` x
      findOdd [y, x, y] `shouldBe` x
      findOdd [y, y, x] `shouldBe` x
  prop "works for lists with five elements" $ \x y ->
    x /= y
      ==> let perms = permutations [x, x, x, y, y]
           in for_ perms $ \xs -> findOdd xs `shouldBe` x

-- Remark: OneOdd's Arbitrary instance makes sure that there
-- is exactly one element that appears an odd number of times
-- prop "works for lists that contain exactly one oddly numbered element
-- " $ \(OneOdd x xs) -> findOdd xs `shouldBe` x
