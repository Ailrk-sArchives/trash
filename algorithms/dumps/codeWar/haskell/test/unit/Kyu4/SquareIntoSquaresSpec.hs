module Kyu4.SquareIntoSquaresSpec where

import           Kyu4.SquareIntoSquares
import           Test.Hspec

spec :: Spec
spec =
  it "should work for some small examples" $ do
    decompose 11 `shouldBe` Just [1, 2, 4, 10]
    decompose 50 `shouldBe` Just [1, 3, 5, 8, 49]
    decompose 4 `shouldBe` Nothing

