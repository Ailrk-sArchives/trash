{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Types.TypeFam2 where

import Control.Monad.ST
import Data.IORef
import Data.STRef

------------------------------------------------------ ------------------------
-- Use type family to replace functional dependency

-- associate open type family
class Mutation m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: (Ref m a) -> m a
  writeRef :: (Ref m a) -> a -> m ()

-- Ref :: (Type -> Type) -> Type is just a type function, and it already
-- uniquely identified the corresponding ref type for m.
--
-- Really if we have type family eariler there is no point of having
-- functional dependency in the first place.
instance Mutation IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance Mutation (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

------------------------------------------------------
-- without type family we need to define the same typeclass with multiparameter
-- typeclass and functional dependency

class Mutation' m r | m -> r where
  newRef' :: a -> m (r a)
  readRef' :: (r a) -> m a
  writeRef' :: (r a) -> a -> m ()

-- this works.
instance Mutation' IO IORef where
  newRef' = newIORef
  readRef' = readIORef
  writeRef' = writeIORef

instance Mutation' (ST s) (STRef s) where
  newRef' = newSTRef
  readRef' = readSTRef
  writeRef' = writeSTRef


------------------------------------------------------ ------------------------
-- Multi paramter type class + type family for implicit conversion
-- say we want add works for int and double. We need int to double essentially.

-- Type relation is a bit tricky especially you introduce lots of bounded
-- type variables. Too many overlapping instances!

class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance (Num a) => Add a a where
  type SumTy a a = a
  add a b = a + b

instance {-# OVERLAPPING #-} Add Double Double where
  type SumTy Double Double = Double
  add a b = a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add a Double where
  type SumTy a Double = Double
  add a b = fromIntegral a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add Double a where
  type SumTy Double a = Double
  add a b = a + fromIntegral b

-- >>> add (1 :: Int) (2 :: Int)
-- >>> add (1.4 :: Double) (2 :: Integer)
-- >>> add (1.4 :: Double) (2 :: Int)
-- >>> add (1 :: Int) (2.5 :: Double)
-- >>> add (1.3 :: Double) (2.5 :: Double)
-- 3
-- 3.4
-- 3.4
-- 3.5
-- 3.8
