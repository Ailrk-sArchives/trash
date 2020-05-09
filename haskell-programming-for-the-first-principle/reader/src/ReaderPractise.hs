module ReaderPractise where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

z' :: [Integer] -> [Integer] -> Integer -> Maybe Integer
z' a b n = lookup n $ zip a b

tupz :: [Integer] -> [Integer]
      -> Integer
      -> Maybe (Integer, Integer)
tupz a b n = (,) <$> Just n <*> z' a b n

tupz' :: [Integer] -> [Integer]
      -> Integer
      -> (Maybe Integer, Maybe Integer)
tupz' a b n = let p = z' a b n in (p, p)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed ct = uncurry' (\x y -> x + y) ct

bolt :: (Bool -> Bool -> Bool)
     -> (Integer -> Bool)
     -> (Integer -> Bool)
     -> Integer
     -> Bool
bolt = liftA2


run :: IO ()
run = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [tupz x y 1, tupz y z 4]
  print $ summed <$> ((,) <$> z' x y 2 <*> z' x y 3)


