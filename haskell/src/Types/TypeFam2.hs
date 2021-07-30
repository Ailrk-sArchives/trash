{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE TypeFamilies           #-}

-- Fun with typeclass
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/typefun.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fassoc-types%2Ffun-with-type-funs%2Ftypefun.pdf
module Types.TypeFam2 where

import           Control.Monad.ST
import           Control.Monad.Trans
import           Data.IORef
import           Data.STRef

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

instance (Monad m, Mutation m, MonadTrans t) => Mutation (t m) where
  type Ref (t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef  -- a lil trick to apply one more argument

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

-- Type relation is a bit tricky when you introduce lots of bounded
-- type variables. Too many overlapping instances!

class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance (Num a) => Add a a where
  type SumTy a a = a
  add a b = a + b

-- have to specialize this instance otherwise it will overlaps with other
-- three.
instance {-# OVERLAPPING #-} Add Double Double where
  type SumTy Double Double = Double
  add a b = a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add a Double where
  type SumTy a Double = Double
  add a b = fromIntegral a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add Double a where
  type SumTy Double a = Double
  add a b = a + fromIntegral b

-- ehh..
instance {-# OVERLAPPING #-} (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add a xs = fmap (add a) xs


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

------------------------------------------------------ ------------------------
-- Graph with data family
-- data family allows you to define data type associate to Graph.

class Graph g where
  type Vertex g
  data Edge g
  src :: Edge g -> Vertex g
  target :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

-- Note, type introduced by `data` is injective.
-- that is, f a ~ f b <=> a ~ b
-- this property needs to be specified by type family.

-- e.g with type family, this is clearly not injective.
type family G2 n where
  G2 Int = Int
  G2 Bool = Char

-- on the other hand, a type introduced by data keyword is always injective.
data G3 a = G3 a
type GTy = G3 Int

newtype G1 = G1 [Int]
instance Graph G1 where
  type Vertex G1 = Int
  data Edge G1 = Edge1 (Maybe Int)  -- the data type associates with the instance.
  src = undefined     -- whatever
  target = undefined
  outEdges = undefined


------------------------------------------------------ ------------------------
-- Type directed optimization
-- this is more like

