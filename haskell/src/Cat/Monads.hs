{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
module Cat.Monads where

{-@ Functor: morphism between categories
    Endofunctor: A functor from a category to itself.
    Natural transformation: 2-morphism between two functors.
@-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans

{-@ monad transformer solves problem of accessing nested monad
    without pattern matching.
@-}

-- simple monad transformer
-- runMaybeT is just a handy accessor.
-- the type impiles MaybeT transforme Maybe into a Monod wrap
-- on top of Maybe.
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- Key point is to notice there is a heirachy of types.
-- MaybeT m a, each one of them is a monad.
-- note under MaybeT it's under m monad of m (Maybe a).
-- so do will bind a Maybe value.
-- notice here do notation behaves like a function return m (Maybe a)
-- MaybeT has no defined >>= yet so it can not use bind directly.
instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing    -> return Nothing
      Just value -> runMaybeT $ f value

-- Monad requirements.
instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

-- some handy class
-- Alternative is like a monoid with different semantic.
instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing -> runMaybeT y
      Just _  -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

{-@ Monad is defined with join, but
    in haskell you get >>= instead. how?
@-}

-- kind signature helps you to tell the kindness of type variable
class (Applicative m) => Monadish (m :: * -> *) where
  merge :: m (m a) -> m a
  bind :: m a -> (a -> m b) -> m b
  bind m f = merge (fmap f m)
  {-# MINIMAL merge #-}

newtype Kleisli m a b = Kleisli (a -> m b)

kbind :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
kbind (Kleisli f) (Kleisli g) = Kleisli $ join . fmap g . f

-- bind' :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- bind' f g = join . fmap g . f

{-@ Comonad is the duality of Monad @-}

-- unsafePerformIO is a cokleisli arrow.
newtype Cokleisli w a b = Cokleisli (w a -> b)

class Functor w => Comonad w where
  (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
  extract :: w a -> a
