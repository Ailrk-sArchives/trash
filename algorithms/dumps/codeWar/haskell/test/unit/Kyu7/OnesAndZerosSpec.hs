module Kyu7.OnesAndZerosSpec where

import Kyu7.OnesAndZeros
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  it "example tests" $ do
    toNumber [0, 0, 0, 1] `shouldBe` 1
    toNumber [0, 0, 1, 0] `shouldBe` 2
    toNumber [1, 1, 1, 1] `shouldBe` 15
    toNumber [0, 1, 1, 0] `shouldBe` 6
