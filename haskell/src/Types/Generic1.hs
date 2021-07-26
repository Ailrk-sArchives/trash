{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Types.Generic1 where

import           Data.Kind
import           GHC.Generics
import           Numeric
import Data.Char

-- basic idea
-- The only thing a generic class does is to create an isomorphism between
-- a and Rep a nothing else.
class MyGeneric a where
  type Rep' a :: Type -> Type
  from' :: a -> (Rep' a) x
  to' :: (Rep' a) x -> a


data Empty deriving Generic

-- Generic representations for type a: Rep a :: Type -> Type
-- from a Rep a x
-- to a

-- This class maps from Generic representations to result.
-- We always need something like this to dispatch the behavior on all
-- representations.
class Encode' f where
  encode' :: f p -> [Bool]

-- dummy case. V1 represents a type without data constructor (Void).
instance Encode' V1 where
  encode' _ = undefined

-- U1 for empty datatype
instance Encode' U1 where
  encode' U1 = []

instance (Encode' f, Encode' g) => Encode' (f :+: g) where
  encode' (L1 x) = False : encode' x
  encode' (R1 x) = True : encode' x

instance (Encode' f, Encode' g) => Encode' (f :*: g) where
  encode' (x :*: y) = encode' x ++ encode' y

instance Encode c => Encode' (K1 i c) where
  encode' (K1 x) = encode x

instance Encode' f => Encode' (M1 i t f) where
  encode' (M1 x) = encode' x

-- the actual class we try to implement
class Encode a where
  encode :: a -> [Bool]
  default encode :: (Generic a, Encode' (Rep a)) => a -> [Bool]
  encode x = encode' (from x)   -- map a to Rep a x and dispatch with encode'

-- defined generic means we have Rep
data Tree a = Leaf
            | Node a (Tree a) (Tree a)

-- Generic works over kind *, Generic1 works on polymorphic kind.
deriving instance Generic (Tree a)
deriving instance Generic1 Tree

deriving instance Show (Tree Int)

t1 = Node (1 :: Int) (Node 2 Leaf Leaf) Leaf
t2 = Node (1 :: Int) (Node 2 Leaf Leaf) (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))

instance Encode Int where
  encode n = fmap f (showIntAtBase 2 intToDigit n "")
    where
      f '0' = False
      f '1' = True

instance Encode a => Encode (Tree a)

-- node how t1 and t2 has completely different shapes, but we can use the same
-- encode function to work on both of them.
-- Trick is we represent adt with datatype, then work on those data types.
-- it's the gist of generic programming. There is nothing more other then that.
n1 = encode t1
n2 = encode t2

-- Generic1
-- bascially the same, but a polymorphic kind version.
-- original Generic ranges over kind *, Generic1 generalized this.
