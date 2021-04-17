-- little utility for doing the assignment.
module ShiftCipher where

import Data.Foldable
import Data.List

c = "ufimeftqnqefarfuyqeufimeftqiadefarfuyqe"

toZn :: Char -> Int
toZn c = fromEnum c - fromEnum 'a'

toChar :: Int -> Char
toChar n = toEnum (n + fromEnum 'a')

shift :: Int -> Char -> Char
shift n c = toChar $ (toZn c + n) `mod` 26

decipher :: [[Char]]
decipher = [shift n <$> c | n <- [1 ..]]

main :: IO ()
main = do
  traverse_ putStrLn (take 16 decipher)
  putStrLn "=="
  putStrLn $ decipher !! 13
