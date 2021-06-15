module Kyu7.MakeAFunctionThatDoesArithmeticSpec where

import Kyu7.MakeAFunctionThatDoesArithmetic
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  it "should work with example tests" $ do
    arithmetic 5 2 Add `shouldBe` 7
    arithmetic 8 2 Subtract `shouldBe` 6
    arithmetic 5 2 Multiply `shouldBe` 10
    arithmetic 8 2 Divide `shouldBe` 4
