module Kyu6.DecipherThis where

import Data.Char

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
