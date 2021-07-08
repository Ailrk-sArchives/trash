module Kyu8.FakeBinary where
import           Test.Hspec
import           Test.QuickCheck


-- Given a string of digits, you should replace any digit below 5 with '0' and any digit 5 and above with '1'.
-- Return the resulting string.

fakeBin :: String -> String
fakeBin xs = f <$> xs
  where
    f x | (read (x : "") :: Integer) < 5 = '0'
      | otherwise = '1'

fakeBin' :: String -> String
fakeBin' = fmap (\c -> if c < '5' then '0' else '1')

-- testing --------------------------------------------------------------------



newtype Number = Number String deriving (Eq, Show)

instance Arbitrary Number where
  arbitrary = Number `fmap` listOf (choose ('0', '9'))

spec :: Spec
spec = do
  it "works for empty strings" $
    fakeBin "" `shouldBe` ""

  it "only returns 0 and 1" $
    property $ \(Number xs) ->
      fakeBin xs `shouldSatisfy` all (`elem` "01")

  it "works for some examples" $ do
    fakeBin "45385593107843568" `shouldBe` "01011110001100111"
    fakeBin "509321967506747" `shouldBe` "101000111101101"
    fakeBin "366058562030849490134388085" `shouldBe` "011011110000101010000011011"
