{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

-- General purpose exitential wrapper
-- https://stackoverflow.com/questions/41865466/library-for-existential-type-wrappers
import           Control.Monad.Cont
import           Data.Kind
import           Unsafe.Coerce

------------------------------------------------------------------------------
-- Exitential Wrapper

data D a = D a deriving Show
data Wrap = forall a. Show a => Wrap (D a)

unwrap :: Wrap -> forall r. (forall a. Show a => D a -> r) -> r
unwrap (Wrap x) k = k x

test1 =  unwrap d f
  where
    d = Wrap $ D 1
    f :: Show a => D a -> String
    f (D x) = show x
-- >>> test1
-- "1"

test2 = unwrap (Wrap . D $ "ha") (\d -> show d ++ "Used here")
-- >>> test2
-- "D \"ha\"Used here"


------------------------------------------------------------------------------
-- bruh
-- forciably coerce. this is really bad.
unwrapUnsafe :: Show a => Wrap -> D a
unwrapUnsafe (Wrap (D a)) = D (unsafeCoerce a)
-- >>> (\(D a) -> show a) $ (unwrapUnsafe (Wrap (D "a")) :: D String)
-- "\"a\""


-------------------------------------------------------------------------------
-- General purpose exitential wrapper

-- we can work with f to whatever shape we want and stuff useful information
-- during the process.
-- data Ex f where
--   Ex :: forall f a. f a -> Ex (f a)

-- ? different?
data Ex f = forall a. Ex (f a)


unwrapEx :: (Ex f) -> forall r. (forall a. f a -> r) -> r
unwrapEx (Ex p) k = k p

-- data Ex f where
--   Ex :: a -> Ex (f a)

----------
-- goal 1. exitentially quantify a type p :: k1 -> k2 -> *
-- we need to convert :: k1 -> k2 -> * to (k1, k2) -> * so it fits into f.

data Uncurry (p :: k1 -> k2 -> Type) ij where
  Uncurry :: { getUncurry :: p i j } -> Uncurry p '(i, j)

data CurriedType i j = CurriedType i j deriving Show
-- >>> :k CurriedType
-- CurriedType :: * -> * -> *

type ExP (p :: k1 -> k2 -> Type) = Ex (Uncurry p)

test3 :: ExP CurriedType -> String
test3 ex = unwrapEx ex (\(Uncurry (CurriedType i j)) -> "asd")

-- We successfully wrapped a type with kind `k1 -> k2 -> Type` into (Ex f)
-- >>> test3 (Ex (Uncurry (CurriedType 1 1)))
-- "asd"

