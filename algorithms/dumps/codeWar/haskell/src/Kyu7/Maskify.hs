-- <Credit Card Mask>
module Kyu7.Maskify (maskify) where
import           Test.Hspec
import           Test.QuickCheck


-----------------------------------------------
-- first version
-----------------------------------------------

maskify :: String -> String
maskify str =
  let l = length str - 4
   in fmap (const '#') (take l str) ++ drop l str

-----------------------------------------------
-- better version
-----------------------------------------------
-- Same idea, but with replicate
-- replicate repeat const second argument n times
-- into [].
maskify' :: String -> String
maskify' str = replicate l '#' ++ drop l str
  where l = length str - 4

-- testing --------------------------------------------------------------------
spec :: Spec
spec = do
  it "should mask the credit card" $ maskify "4556364607935616" `shouldBe` "############5616"
  it "should mask another number" $ maskify "64607935616" `shouldBe` "#######5616"
  it "should mask a short number" $ maskify "616" `shouldBe` "616"
  it "should mask a single character" $ maskify "1" `shouldBe` "1"
  it "should mask an empty string" $ maskify "" `shouldBe` ""
  it "should mask your pet" $ maskify "Skippy" `shouldBe` "##ippy"
  it "should mask batman" $
    maskify "Nananananananananananananananana Batman!" `shouldBe` "####################################man!"
  it "shouldn't change the length" $
    property $ \x ->
      length x == length (maskify x)
