module Ch9 where

import Data.Char

-- list pattern matching
myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

-- with Maybe
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- enum
tokenize :: Char -> String -> [String]
tokenize delimiter = splitWords . dropWhile (== delimiter)
    where
        splitWords "" = []
        splitWords s =
            let word = takeWhile (/= delimiter) s
                (_, rest) = splitAt (length word) s
            in word : splitWords (dropWhile (== delimiter) rest)

-- list comperhension
hasVow :: String -> Maybe [Char]
hasVow xs = out res
    where
        res = [x | x <- xs, elem x "aeiou"]
        out s
          | s == [] = Nothing
          | otherwise = Just s

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

len :: [a] -> Integer
len [] = 0
-- pattern match on the : constructor.
len (_:xs) = 1 + len xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
  | pred x = x : myFilter pred xs
  | otherwise = myFilter pred xs

-- exercises
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

capAll :: String -> String
capAll "" = ""
capAll (x:xs) = toUpper x : capAll xs

capHead :: String -> Maybe Char
capHead "" = Nothing
capHead x = Just (toUpper . head $ x)

-- practices
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr. map f $ xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f (x:xs) = concat . go f $ (x:xs)
    where go f [] = []
          go f (x:xs) = f x : go f xs

squishAgain :: [[a]] -> [a]
squishAgain nestedList = squishMap id nestedList




