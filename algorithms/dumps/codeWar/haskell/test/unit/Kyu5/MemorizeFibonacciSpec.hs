module Kyu5.MemorizeFibonacciSpec where

import Kyu5.MemorizeFibonacci
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "Fixed tests" $ do
    fibonacci 70 `shouldBe` 190392490709135
    fibonacci 60 `shouldBe` 1548008755920
    fibonacci 50 `shouldBe` 12586269025
