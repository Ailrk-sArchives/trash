{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Types.Variance where

-- given a -> b, can we have t a -> t b?
-- or can we have functor for t?

-- Covariant: (a -> b) -> (t a -> t b)
-- Contravariant: (a -> b) -> (t b -> t a)
-- Invariant: (a -> b) cant be lifted into t.


class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

-- a and be needs to be isomorphic to have f a -> f b
class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

-- first, all algebraic data types can be represented by
-- Either, (,) and (->)


data Signed
  = Pos  -- +
  | Neg  -- -

data Variance = Variance Signed Signed
data Iso a b = Sum (Either a b)
             | Prod (a, b)
             | Map (a -> b)

variance :: Iso a b -> Variance
variance (Sum _)  = Variance Pos Pos
variance (Prod _) = Variance Pos Pos
variance (Map _)  = Variance Neg Pos

-- only functions argument makes a type contravariant.
-- only T1 and T5 has functor

-- a is +
newtype T1 a = T1 (Int -> a) deriving Functor

-- a is -, contravariant
newtype T2 a = T2 (a -> Int)

-- a is both + and -, invariant
newtype T3 a = T3 (a -> a)

-- a is + * - -> -, contravariant.
newtype T4 a = T4 ((Int -> a) -> Int)

-- a is - * - -> + covariatn.
newtype T5 a = T5 ((a -> Int) -> Int) deriving Functor
