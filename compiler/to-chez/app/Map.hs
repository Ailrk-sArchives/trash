{-# LANGUAGE BangPatterns #-}

-- just make a simple map for the environment.

module Map where

import Prelude hiding (lookup, map, null)

type Size = Int

data Map k a
  = Tip
  | Bin {-# UNPACK #-} !Size k a !(Map k a) !(Map k a)
  deriving (Show)

lookup :: Ord k => Map k a -> k -> Maybe a
lookup Tip _ = Nothing
lookup (Bin _ k' a m1 m2) k
  | k == k' = Just a
  | k > k' = lookup m2 k
  | otherwise = lookup m1 k
{-# INLINE lookup #-}

empty :: Ord k => Map k a
empty = Tip
{-# INLINE empty #-}

null :: Ord k => Map k a -> Bool
null Tip = True
null _ = False
{-# INLINE null #-}

map :: Ord k => (a -> b) -> Map k a -> Map k b
map _ Tip = Tip
map f (Bin sz k a m1 m2) = Bin sz k (f a) (map f m1) (map f m2)
{-# INLINE map #-}

instance Ord k => Functor (Map k) where
  fmap = map

member :: Ord k => k -> Map k a -> Bool
member k Tip = False
member k (Bin _ k' _ m1 m2) =
  k == k'
    || if k > k'
      then member k m2
      else member k m1
{-# INLINE member #-}

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a Tip = Bin 1 k a Tip Tip
insert k a (Bin sz k' a' m1 m2)
  | k == k' = error "already exits"
  | k > k' = Bin (sz + 1) k' a' m1 (insert k a m2)
  | otherwise = Bin (sz + 1) k' a' (insert k a m1) m2
{-# INLINE insert #-}

foldr :: Ord k => (a -> b -> b) -> b -> Map k a -> Map k b
foldr = undefined
{-# INLINE foldr #-}
