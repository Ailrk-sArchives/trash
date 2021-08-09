module Ch18Monad where

-- Nope Monad
data Nope a = NopeDog deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDog = NopeDog

instance Applicative Nope where
  pure _ = NopeDog
  NopeDog <*> NopeDog = NopeDog

instance Monad Nope where
  return _ = NopeDog
  NopeDog >>= _ = NopeDog

-- Either' Monad


-- PhEither Monad
-- data PhEither b a =
--     PLeft a
--   | PRight b
--   deriving (Eq, Show)

-- instance Functor (PhEither b) where
--     fmap _ (PRight a) = (PRight a)
--     fmap f (PLeft a) = PLeft (f a)

-- instance Applicative (PhEither b) where
--     pure x = PLeft x
--     (PLeft f) <*> (PLeft a) = PLeft $ f a
--     (PRight a) <*> _ = PRight a
--     _ <*> (PRight a) = PRight a

-- instance Monad (PhEither b) where
--     return = pure
--     (PLeft a) >>= f = f a
--     (PRight a) >>= _ = PRight a


-- Identity

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure x = Identity x
    (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

-- List Monad
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil

instance Semigroup (List a) where
    Nil <> Nil = Nil
    as <> Nil = as
    Nil <> as = as
    (Cons a as) <> bs =
        (Cons a (as <> bs))

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a as) =
        Cons (f a) (fmap f as)

instance Applicative List where
    pure x = Cons x Nil
    (Cons f fs) <*> (Cons a as) =
        Cons (f a) (fs <*> as)
    Nil <*> _ = Nil
    _ <*> Nil = Nil

instance Monad List where
    return = pure
    Nil >>= f = Nil
    (Cons a as) >>= f =
        (f a) <> (as >>= f)

-- impelment functions with monad and functor.
j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = do
    x' <- f x
    ((:) x') <$> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id

