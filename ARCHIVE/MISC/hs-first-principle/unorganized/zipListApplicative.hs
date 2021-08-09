module ZipListApplicative where

import Test.QuickCheck

-- List Applicative
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
    Nil <> as = as
    as <> Nil = as
    (Cons a as) <> bs = Cons a $ as <> bs

instance Monoid (List a) where
    mempty = Nil

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

take' :: Int -> List a -> List a
take' idx xs = go idx xs Nil 0
    where
        go :: Int -> List a -> List a -> Int -> List a
        go i Nil rs count = if count < i then Nil else rs
        go i (Cons a as) rs count
          | count == idx = rs
          | otherwise = go idx as (rs <> pure a) (count + 1)

-- ZipList
newtype ZipList a =
    ZipList (List a)
    deriving (Eq, Show)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
    pure x = ZipList (pure x)
    ZipList Nil <*> _ = ZipList Nil
    _ <*> ZipList Nil = ZipList Nil
    ZipList (Cons f fs) <*> ZipList (Cons x xs) =
        ZipList (Cons (f x) (fs <*> xs) )


