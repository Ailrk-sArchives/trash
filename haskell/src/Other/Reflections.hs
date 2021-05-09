{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Other.Reflections where

import           Data.Proxy
import           Data.Reflection

-- https://www.tweag.io/blog/2017-12-21-reflection-tutorial/
-- typeclass reflection.
-- use a value as a type class instance.

newtype SortedList a = Sorted [a] deriving (Show, Eq)

forget :: SortedList a -> [a]
forget (Sorted l) = l

nil :: SortedList [a]
nil = Sorted []

singleton :: a -> SortedList a
singleton a = Sorted [a]

-- The point is this version is not particularly satisfying.
-- The reason is we can't enforce the fact that value in Sorted
-- is acutallly sorted.
mergeList :: Ord a => SortedList a -> SortedList a -> SortedList a
mergeList (Sorted left) (Sorted right) = Sorted (merge' left right)
  where
    merge' :: Ord a => [a] -> [a] -> [a]
    merge' [] rs = rs
    merge' ls [] = ls
    merge' ls@(l:ls') rs@(r:rs')
      | l <= r = l : (merge' ls' rs)
      | otherwise = r : (merge' ls rs')

fromList :: Ord a => [a] -> SortedList a
fromList [] = Sorted []
fromList [a] = singleton a
fromList xs = mergeList left right
  where
    left = fromList left'
    right = fromList right'
    (left', right') = splitAt (length xs `div` 2) xs


sort :: Ord a => [a] -> [a]
sort l = forget (fromList l)

-- motivation
-- use a value as a typeclass instance

newtype ReflectedOrd s a = ReflectedOrd { unreflectOrd :: a }

reflectOrd :: Proxy s -> a -> ReflectedOrd s a
reflectOrd _ a = ReflectedOrd a

data ReifiedOrd a = ReifiedOrd
  { reifiedEq      :: a -> a -> Bool
  , reifiedCompare :: a -> a -> Ordering
  }

instance Reifies s (ReifiedOrd a) => Eq (ReflectedOrd s a) where
  (==) (ReflectedOrd x) (ReflectedOrd y) =
    reifiedEq (reflect (Proxy :: Proxy s)) x y

instance Reifies s (ReifiedOrd a) => Ord (ReflectedOrd s a) where
  compare (ReflectedOrd x) (ReflectedOrd y) =
    reifiedCompare (reflect (Proxy :: Proxy s)) x y

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy ord xs =
  reify (fromCompare ord) $ \p ->
    map unreflectOrd . sort . map (reflectOrd p) $ xs


fromCompare :: (a -> a -> Ordering) -> ReifiedOrd a
fromCompare ord = ReifiedOrd
  { reifiedEq = \x y -> ord x y == EQ
  , reifiedCompare = ord
  }
