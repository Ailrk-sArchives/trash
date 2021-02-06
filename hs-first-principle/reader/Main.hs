{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

-- Let's show that fmap a function to another function
-- is the same as func comp when two functions has
-- the same type of parameter (same funcotorial context).

comp :: forall a b c. (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

compEqfmap x = fg' x == fg'' x
  where
    f = (+ 10)
    g = (* 2)
    fg' = f `comp` g
    fg'' = f <$> g

-- Reader: a way of stringing functions together when all
-- those functions are awaiting one input from a shared environment.
-- Like create a little scope with a "global variable."
-- It can avoid passing the same parameter over and over again.

newtype Reader r a = Reader {runReader :: r -> a}

-- the Functor of Reader is really just a wrapper of fmap. --
-- and since fmap is the same as (.) in terms of fuction composition.
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

-- Exercise
ask :: Reader a a
ask = Reader $ id

-- The Applicative of function --
-- For functions Applicative will
-- do a function composition underline the
-- common parameter.
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

-- Exercise
liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader $ f

-- The Monad of function --
instance Monad (Reader r) where
  return = pure
  m >>= k =
    Reader $ \r ->
      runReader (k (runReader m r)) r

-- Change what comes below but not above. --
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

withReaderT ::
  forall r' r m a.
  (r' -> r) ->
  ReaderT r m a -> -- func that modify the environment
  ReaderT r' m a -- computation to run in the modified env
withReaderT f m = ReaderT $ runReaderT m . f


main :: IO ()
main = putStrLn "Hello, Haskell!"
