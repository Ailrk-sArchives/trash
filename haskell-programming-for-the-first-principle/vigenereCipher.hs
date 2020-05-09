module VigenereCipher where
import Data.Char

shift :: Char -> Int -> Char
shift char n = chr . modAplhaBete (ord char) $ n
    where modAplhaBete :: Int -> Int -> Int
          modAplhaBete id n = (+97) . mod (id - 97 + n) $ 26

shiftChar :: Char -> Int
shiftChar key = (ord key) - 97

align :: Int -> Int -> Int
align strIdx keyLen = mod strIdx keyLen

encodeVingenere :: String -> String -> String
encodeVingenere key (x:xs) = go key (x:xs) 0
    where
        go _ "" _ = ""
        go k (a:as) count =
            shift a
                  (shiftChar $ key !! align count (length k))
            : go k as (count + 1)




