-- <Even or Odd>
module Kyu7.EvenOrOdd where
import Test.Hspec
import Test.QuickCheck



-- for test suit set up
evenOrOdd :: Integral a => a -> String
evenOrOdd a = if mod a 2 == 0 then "Even" else "Odd"

-- testing --------------------------------------------------------------------
spec :: Spec
spec = do
  it "Evens:" $ do
    evenOrOdd 2 `shouldBe` "Even"
    evenOrOdd 0 `shouldBe` "Even"
  it "Odds" $ do
    evenOrOdd 7 `shouldBe` "Odd"
    evenOrOdd 1 `shouldBe` "Odd"
