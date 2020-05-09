module Ciphers where
import Data.Char

modAplhaBete :: Int -> Int -> Int
modAplhaBete identity n = (+97) . mod (identity - 97 + n) $ 26

shift :: Char -> Int -> Char
shift char = chr . modAplhaBete (ord char)

encodeCeaser :: String -> Int -> String
encodeCeaser "" _ = ""
encodeCeaser s shiftVal =
    let (x:xs) = s
     in shift x shiftVal : encodeCeaser xs shiftVal

ceaser :: String -> String
ceaser s = encodeCeaser s 13

unceaser :: String -> String
unceaser s = encodeCeaser s (-13)
