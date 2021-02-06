 module MonadCountMe where

data CountMe a =
    CountMe Integer a
    deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
    return = pure
    CountMe n a >>= f =
        let CountMe n' b = f a
         in CountMe (n + n') b  -- careful about the monoid part of Monad


-- Kleisli composition
mcomp :: Monad m =>
        (b -> m c)
     -> (a -> m b)
     -> a -> m c
mcomp f g a = g a >>= f

