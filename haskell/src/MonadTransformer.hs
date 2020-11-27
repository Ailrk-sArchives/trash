module MonadTransformer where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- monad transformer solves problem of accessing nested monad
-- without pattern matching.

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
      Nothing -> return Nothing
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
      Just _ -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)
