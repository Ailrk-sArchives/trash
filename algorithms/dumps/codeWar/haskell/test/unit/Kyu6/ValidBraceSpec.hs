module Kyu6.ValidBraceSpec where

import Kyu6.ValidBrace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf (printf)

spec :: Spec
spec =
  it "should work for some examples" $ do
    validBraces "()" `shouldBe` True
    validBraces "[([)" `shouldBe` False
    validBraces "())({}}{()][][" `shouldBe` False
    validBraces "({})[({})]" `shouldBe` True
