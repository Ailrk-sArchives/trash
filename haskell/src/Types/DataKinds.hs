{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Types.DataKinds where

-- promote data to type.

import           Data.Proxy
import           GHC.TypeLits

data TypeList l where
  Nil :: TypeList '[]
  (:::) :: a -> TypeList l -> TypeList (a ': l)

infixr :::

-- >>> 1 ::: "Hello" ::: 1.1 :: TypeList '[Int, String, Double]

type family Param f :: [*] where
  Param (a -> f) = a ': Param f
  Param r = '[]

type family Result f :: * where
  Result (a -> f) = Result f
  Result r = r

class (Param f ~ l, Result f ~ r) => ToTypeList f l r where
  translate :: f -> TypeList l -> r

instance (ToTypeList f l r) => ToTypeList (a -> f) (a ': l) r where
  translate f (a ::: l) = translate (f a) l

instance (Param f ~ '[], Result f ~ r, f ~ r) => ToTypeList f '[] r where
  translate r Nil = r

-------------------------------------------------------------------------------

newtype Pointer (align :: Nat) = Pointer Integer deriving Show

zeroPtr :: Pointer n
zeroPtr = Pointer 0

inc :: Pointer align -> Pointer align
inc (Pointer n) = Pointer (n + 1)

-- get the actual pointer value.
ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer p) = p * natVal (Proxy :: Proxy align)

n1 = ptrValue (inc $ zeroPtr :: Pointer 4)
n2 = ptrValue (inc $ zeroPtr :: Pointer 8)
n3 = ptrValue ((inc . inc . inc) $ zeroPtr :: Pointer 16)

-- check if a given integer is a properly aligned pointer
maybePtr :: forall align. KnownNat align => Integer -> Maybe (Pointer align)
maybePtr p
  | remainder == 0 = Just (Pointer quotient)
  | otherwise = Nothing
  where
    (quotient, remainder) = divMod p (natVal (Proxy :: Proxy align))

n4 = maybePtr 24 :: Maybe (Pointer 8)
n5 = maybePtr 100 :: Maybe (Pointer 12)
n6 = maybePtr 120 :: Maybe (Pointer 13)
