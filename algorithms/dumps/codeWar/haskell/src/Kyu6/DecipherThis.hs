module Kyu6.DecipherThis where
import           Data.Char
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Printf           (printf)


decipherThis :: String -> String
decipherThis = unwords . fmap decipher1 . words

decipher1 :: String -> String
decipher1 word =
    case count word of
      0 -> []
      1 -> chead : []
      2 -> chead : cend : []
      _ -> chead : cend : (init . tail) t ++ c1 : []
  where
    count = (+ 1) . length . (dropWhile isDigit)
    t = dropWhile isDigit word
    chead = chr $ (read $ takeWhile isDigit word :: Int)
    c1 = head $ t
    cend = last word

-- testing --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Basic Tests" $
    it "should work with basic tests" $ do
      decipherThis "65 119esi 111dl 111lw 108dvei 105n 97n 111ka" `shouldBe` "A wise old owl lived in an oak"
      decipherThis "84eh 109ero 104e 115wa 116eh 108sse 104e 115eokp" `shouldBe` "The more he saw the less he spoke"
      decipherThis "84eh 108sse 104e 115eokp 116eh 109ero 104e 104dare" `shouldBe` "The less he spoke the more he heard"
      decipherThis "87yh 99na 119e 110to 97ll 98e 108eki 116tah 119esi 111dl 98dri" `shouldBe` "Why can we not all be like that wise old bird"
      decipherThis "84kanh 121uo 80roti 102ro 97ll 121ruo 104ple" `shouldBe` "Thank you Piotr for all your help"
