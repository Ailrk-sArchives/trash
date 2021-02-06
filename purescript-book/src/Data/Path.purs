module Data.Path where

import Prelude

import Control.MonadPlus (guard)
import Data.Array (concat, concatMap, filter, null, tail, (..))
import Data.Foldable (foldl, foldr, product)
import Data.Maybe (fromMaybe)


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 2)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- not tail recursive.
length :: forall a. Array a -> Int
length arr
    | null arr = 1
    | otherwise = 1 + (length $ fromMaybe [] $ tail arr)

showRange :: forall a b. Show b => Array a
                                -> Array a
                                -> (a -> Array b)
                                -> Array String
showRange list1 list2 fn = show <$> (concatMap fn $ concat [list1, list2])

pairs' :: Int -> Array (Array Int)
pairs' n = concatMap (\i ->
    map (\j -> [i, j]) (i..n)) (1..n)

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs' n)

-- do notation for list comprehension.
factors2 :: Int -> Array (Array Int)
factors2 n = filter (\xs -> product xs == n) $ do
    i <- 2 .. n
    j <- i .. n
    pure [i, j]

factors3 :: Int -> Array (Array Int)
factors3 n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n   -- remember if param is false guard'll block >>=
    pure [i, j]

-- folds
symetric :: Array Int -> String
symetric arr = (foldl (\acc n -> acc <> show n) "" arr)
            <> (foldr (\n acc -> acc <> show n) "" arr)

-- tail recursion
tailrecFact :: Int -> Int -> Int
tailrecFact 0 acc = acc
tailrecFact n acc = tailrecFact (n - 1) (acc * n)

-- accumulator parameter (non tail rec -> tail rec)
lengthTailRec :: forall a. Array a -> Int
lengthTailRec arr = length' arr 0
    where
      length' :: Array a -> Int -> Int
      length' arr' acc =
          if null arr'
              then 0
              else length' (fromMaybe [] $ tail arr') (acc + 1)

-- prefer folds to explicit recursion.
reverse' :: forall a. Array a ->  Array a
reverse' = foldr (\x xs -> xs <> [x]) []
