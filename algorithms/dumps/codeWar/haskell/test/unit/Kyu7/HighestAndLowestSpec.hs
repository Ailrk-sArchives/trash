module Kyu7.HighestAndLowestSpec where

import Kyu7.HighestAndLowest
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  it "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" $
    highAndLow "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" `shouldBe` "542 -214"
