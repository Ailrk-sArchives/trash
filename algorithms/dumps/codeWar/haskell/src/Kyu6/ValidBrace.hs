-- <Valid Braces>
module Kyu6.ValidBrace where

-- 2019-11-19
--------------------------------------
-- first attempt
--------------------------------------
-- Note
-- head is opposite to last bc they return element
-- tail is opposite to init bc they return list
-- very wierd
pair :: Char -> Char -> Bool
pair '(' ')'= True
pair '[' ']'= True
pair '{' '}'= True
pair _ _ = False

validBraces :: String -> Bool
validBraces as = go as []
  where go [] [] = True
        go [] _ = False
        go (x:xs) [] = go xs [x]
        go (x:xs) acc =
          if pair (last acc) x
             then go xs (init acc)
          else go xs (acc ++ [x])

--------------------------------------
-- clever solution
--------------------------------------
-- Note
-- Use fold flexibly can reduce a lot of works.
validBraces' :: String -> Bool
validBraces' s = "" == foldr collapse [] s
  where collapse '(' (')':xs) = xs
        collapse '[' (']':xs) = xs
        collapse '{' ('}':xs) = xs
        collapse x xs = x:xs


