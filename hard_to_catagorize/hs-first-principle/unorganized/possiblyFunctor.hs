module PossiblyFunctor where

-- Maybe Functor
data Possibly a =
    LolNope
  | Yeet a
  deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeet a) = Yeet $ f a

-- Either Functor
data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Or a) where
    fmap f (Second b) = Second $ f b
    fmap _ (First a) = First a

-- Impossible, because the type argument a is part of the Functor structure.
-- instance Functor (Or a) where
--     fmap _ (Second b) = Second b
--     fmap f (First a) = First $ (f a)
