{-# LANGUAGE DeriveFunctor #-}
module Other.RecursionScheme2 where

-- factoring recursion out using pattern functors and a fixed point wrapper.

-- factoring recursion out using pattern functors and a fixed point wrapper.
import           Data.Fix              hiding (cata)
import           Data.Functor.Foldable
import           Data.List.Ordered     (merge)
import           Prelude               hiding (Foldable, succ)

-- 1. recursion schemes work to recursive data types

-- Idea: factor out the recursion with a base structure, then back it in
--       with a recursive wrapper type

-- parameter functor.
data NatF r = ZeroF | SuccF r deriving (Show, Functor)
data ListF' a r = NilF' | ConsF' a r deriving (Show, Functor)
data TreeF' a r = LeafF' a | Node' r r deriving (Show, Functor)

type Nat = Fix NatF
type List a = Fix (ListF' a)
type Tree a = Fix (TreeF' a)

-- find the Fix point of the type.
-- Fix NatF -> NatF (Fix NatF) -> ...

zero :: Nat
zero = Fix ZeroF

succ :: Nat -> Nat
succ = Fix . SuccF

nil :: List a
nil = Fix NilF'

cons :: a -> List a -> List a
cons a as = Fix (ConsF' a as)

-- >>> succ (succ (succ zero))

-------------------------------------------------------------------------------
-- recursive && corecursive


-------------------------------------------------------------------------------
-- catamorphism
-- the banana bracket
natsum :: Nat -> Int
natsum = cata algebra
  where
    algebra ZeroF     = 0
    algebra (SuccF n) = n + 1

-- >>> natsum (succ (succ zero))
-- 2
