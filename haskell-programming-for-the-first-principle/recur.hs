module Recur where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialGuard :: Integer -> Integer
factorialGuard n
  | n == 0 = 1
  | otherwise = n * factorialGuard (n - 1)

-- recursion
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n

-- use type Maybe
fMaybe :: Bool -> Maybe Int
fMaybe False = Just 0
fMaybe _ = Nothing

-- fibonacci
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- division with recursion
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

-- user defined data type to handle exceptions.
data DividedResult =
    Result Quotient
  | DividedByZero
  deriving (Eq, Show)

dividedBy :: Numerator -> Denominator -> DividedResult
dividedBy num deno = go num deno 0
    where go n d count
            | deno == 0 = DividedByZero
            | n < d = Result count
            | otherwise = go (n - d) d (count + 1)

-- go is an idiom of haskell

sumUp :: (Eq a, Num a) => a -> a
sumUp n
    | n == 1 = 1
    | otherwise = (+) n . sumUp $ (n - 1)

mulBy :: (Integral a) => a -> a -> a
mulBy a b = go a b 0
    where go a b product
            | b == 0 = product
            | otherwise = go a (b - 1) (product + a)

-- McCarthy 91 function
mc91 :: Integer -> Integer
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 . mc91 $ n + 11

