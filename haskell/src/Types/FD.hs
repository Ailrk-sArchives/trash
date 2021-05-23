{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Types.FD where

class Coll c e | c -> e where
  empty :: c
  insert :: c -> e -> c
  member :: c -> e -> Bool


-- [a] determines e should be a.
instance Eq a => Coll [a] a where
  empty = []
  insert xs a = a : xs
  member xs e = e `elem` xs

-- type checker infer to c -> e -> e -> c instead of c -> e1 -> e2 -> c
ins2 xs a b = insert (insert xs a) b

-- without functional dependencies even with same c in the context we can't say
-- what type e will be, so you can only assume different instances of e are different types.
class Coll' c e where
  empty' :: c
  insert' :: c -> e -> c
  member' :: c -> e -> Bool


