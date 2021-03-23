module Ch7 where

-- Ex variety pack
k :: (a, b) -> a
k (x, y) = x

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))


-- Case expressions
functionC x y =
    case isBigger of
      True -> x
      False -> y
    where isBigger = x > y

ifEvenAdd2 n =
    case even n of
      True -> n + 2
      False -> n

nums x =
    case compare x 0 of
      LT -> -1
      GT -> 1
      _ -> 0

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y t =
    case t of
        True -> x
        False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y t
  | t = x
  | otherwise = y

foldBoolPatternMatch :: a -> a -> Bool -> a
foldBoolPatternMatch x y True = x
foldBoolPatternMatch x y False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f . fst $ (a, c), c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show

-- ex pattern matching is about the data.
data SumOfThree a b c =
    FirstPossible a
  | SecondPossible b
  | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 0
sumToInt (ThirdPossible _) = 0

