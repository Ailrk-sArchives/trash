{-# LANGUAGE MultiParamTypeClasses #-}

module ThinkingWithTypes where

{-@ 1. the algebra behind type @-}

-- what's the cardinaltity?
data Void'  -- 0

data Unit = Unit () -- 1

-- two types with the same cardialitieswll always be isomorphic to each other.
class Isomorphism s t where
  to :: s -> t
  from :: t -> s


-- from . to = id, to . from = id
data Spin = Up | Down

instance Isomorphism Spin Bool where
  to Up   = True
  to Down = False
  from True  = Up
  from False = Down


-- if cardinality == n, there are n! Isomorphisms!

-- cardinarlity and sumtype, product type
-- |Either a b| = |a| + |b|
-- |(a, b) = |a| * |b|
-- |Maybe a| = |a| + 1

{-@ prove some truth @-}
-- a x 1 = a
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, _) = a

-- a + 0 = a
sumUnitTo :: Either a Void' -> a
sumUnitTo (Left a) = a
sumUnitTo (Right _) = error "no"

sumUnitFrom :: a -> Either a Void'
sumUnitFrom = Left

-- function a -> b has cardinality |b| ^ |a|

-- |Either Bool (Bool, Maybe Bool) -> Bool|
-- =  2 + (2*2 + 1)^2 = 27
