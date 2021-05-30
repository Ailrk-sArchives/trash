{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Types.Cardinality where

{-@ 1. the algebra behind type @-}

-- what's the cardinaltity?
data Void'  -- 0

data Unit = Unit () -- 1

-- this function will never be actually called, because there is
-- no data for type Void'
absurd :: Void' -> a
absurd = undefined

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
sumUnitTo (Left a)  = a
sumUnitTo (Right v) = absurd v

sumUnitFrom :: a -> Either a Void'
sumUnitFrom = Left

-- function a -> b has cardinality |b| ^ |a|
-- for Bool -> Bool, we have the cardinality = |2| ^ |2| = 4.
-- You can think a -> b as give a with |a|, for each a' \in a
-- there are a b` \in |b| s.t a' -> b' is a value of the type.
-- here are four possible inhabinants of the type:
id' :: Bool -> Bool
id' True  = True
id' False = False

not' :: Bool -> Bool
not' True  = False
not' False = True

constTrue = const True
constFalse = const False

-- |Either Bool (Bool, Maybe Bool) -> Bool|
-- =  2 + (2*2 + 1)^2 = 27
