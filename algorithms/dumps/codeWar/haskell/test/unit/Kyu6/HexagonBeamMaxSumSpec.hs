module Kyu6.HexagonBeamMaxSumSpec where

import Kyu6.HexagonBeamMaxSum
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf (printf)

spec :: Spec
spec = do
  it "Sample tests" $ do
    maxHexagonBeam 2 [5, 8, 3, 8] `shouldBe` 24
    maxHexagonBeam 3 [1, 3, 5, 7] `shouldBe` 23
    maxHexagonBeam 4 [2, 4, 6, 8] `shouldBe` 34
    maxHexagonBeam 5 [1, 0, 4, -6] `shouldBe` 9
    maxHexagonBeam 5 [2] `shouldBe` 18
