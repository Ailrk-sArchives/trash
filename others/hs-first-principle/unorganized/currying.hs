module Currying where


nonsense :: Bool -> Integer
nonsense True = 999
nonsense False = 111

curriedF :: Integer -> Bool -> Integer
curriedF i b = i + (nonsense b)

uncurriedF :: (Integer, Bool) -> Integer
uncurriedF (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)

-- cuuring and uncurrying
currying :: ((a, b) -> t) -> a -> b -> t
currying f a b = f (a, b)

addUncurried (a, b) = a + b
addCurried = currying addUncurried

g :: a -> b -> c -> b
g a b c = b

f :: (Num a, Num b) => a -> b -> b
f a b = b
-- f 1 1.0 :: Num b => b. the type system take the most polymorphic inference.
-- so f 1.0 1 :: Num b => b.

jackel :: (Ord a, Eq b) => a -> b -> a
jackel a b = a

-- parametricity
-- Two ways to implement puer a -> a -> a
f1 :: a -> a -> a
f1 a b = a

f2 :: a -> a -> a
f2 a b = b

-- implement a -> b -> b
f3 :: a -> b -> b
f3 a b = b

