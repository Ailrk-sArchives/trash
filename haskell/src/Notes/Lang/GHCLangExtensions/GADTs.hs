{-# LANGUAGE GADTs #-}
-- Generalised Algebraic Data Structures

module Notes.Lang.GHCLangExtensions.GADTs where

-- some random typeclass need to be implemented
class Vector a where
  add :: a -> a -> a

-- some random typeclass
class C a where
  d :: a -> a -> a

-------------------------------------------------------
-- normal way to add type constrain in instance implement
newtype T a = Cons a deriving (Eq, Show)

instance C (T a) where
  d (Cons x) (Cons y) = Cons y

instance (C a) => Vector (T a) where
  add (Cons x) (Cons y) = Cons (d x y)


-------------------------------------------------------
-- what if I want to impose the type constraint on a?
--                                                 ^
-- use GADTs extension to add implicit context to data
-- constructor
-- now Cons requres context C on a instread of T'
data T'' a where
  Cons'' ::(C a, Show a) => a -> T'' a

instance Show (T'' a) where
  show (Cons'' x) = "T''" ++ show x

instance C (T'' a) where
  d (Cons'' a) (Cons'' b) = Cons'' a

instance Vector (T'' a) where  -- now don't need to specify here.
  add (Cons'' x) (Cons'' y) = Cons'' (d x y)
