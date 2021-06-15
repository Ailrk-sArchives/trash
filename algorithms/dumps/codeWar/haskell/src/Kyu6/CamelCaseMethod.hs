-- <CamelCase Method>
-- Write a function that takes a string of braces, and determines if the order
-- of the braces is valid. It should return true if the string is valid, and
-- false if it's invalid.

-- This Kata is similar to the Valid Parentheses Kata, but introduces new
-- characters: brackets [], and curly braces {}. Thanks to @arnedag for the idea!

-- All input strings will be nonempty, and will only consist of parentheses,
--  brackets and curly braces: ()[]{}.
-- What is considered Valid?

-- A string of braces is considered valid if all braces are matched with
-- the correct brace.
module Kyu6.CamelCaseMethod where

import Data.Char

-- 2019-11-19
----------------------------------------
-- first attempt
----------------------------------------
camelCase :: String -> String
camelCase str = concat $  -- why not use (x:xs) ??
  (\(x, y) -> (toUpper <$> x) ++ y) . splitAt 1 <$> words str

camelCaseRefactor :: String -> String
camelCaseRefactor str = concat $ (\(x:xs) -> toUpper x:xs) <$> words str

----------------------------------------
-- bestPractice
----------------------------------------
camelCase' :: String -> String
camelCase' = concatMap (\(x:xs) -> toUpper x:xs) . words

----------------------------------------
-- monad version
----------------------------------------
camelCase'' :: String -> String
camelCase'' str = do
  x:xs <- words str
  toUpper x:xs

