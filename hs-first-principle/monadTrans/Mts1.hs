module Mts1 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Char

newtype MaybeT' m a = MaybeT' {runMaybeT' :: m (Maybe a)}

instance Monad m => Monad (MaybeT' m) where
  return = MaybeT' . return . return
  x >>= f = MaybeT' $ do
    maybeval <- runMaybeT' x
    case maybeval of
      Nothing -> return Nothing
      Just val -> runMaybeT' $ f val

instance Monad m => Applicative (MaybeT' m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT' m) where
  fmap = liftM

instance Monad m => Alternative (MaybeT' m) where
  empty = MaybeT' $ return Nothing
  x <|> y = MaybeT' $ do
    maybeval <- runMaybeT' x
    case maybeval of
      Nothing -> runMaybeT' y
      Just _ -> return maybeval

instance Monad m => MonadPlus (MaybeT' m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans MaybeT' where
  lift = MaybeT' . (liftM Just)

-- use mtf
isValid :: String -> Bool
isValid s =
  length s >= 8
    && any isAlpha s
    && any isNumber s
    && any isPunctuation s

getPassphrase :: MaybeT' IO String
getPassphrase = do
  s <- lift getLine --  lift IO in IO String into IO (Maybe String).
  guard (isValid s)
  return s

askPassphrase :: MaybeT' IO ()
askPassphrase = do
  lift $ putStrLn "Insert your new passphrase"
  value <- getPassphrase
  lift $ putStrLn "Storing in database..."

askPassphrase' :: MaybeT' IO ()
askPassphrase' = do
  lift $ putStrLn "Insert your new passphrase"
  value <- msum $ repeat getPassphrase
  lift $ putStrLn "Storing in database"

newtype Identity a = Identity {runIdentity :: a}

instance Monad Identity where
  return a = Identity a
  m >>= f = f (runIdentity m)

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Functor Identity where
  fmap = liftM

newtype IdentityT m a = IdentityT {runIdentityT :: m (Identity a)}

instance Monad m => Monad (IdentityT m) where
  return = IdentityT . return . return
  x >>= f = IdentityT $ do
    identityval <- runIdentityT x
    runIdentityT $ f (runIdentity identityval)

instance Monad m => Applicative (IdentityT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (IdentityT m) where
  fmap = liftM

newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State x) >>= f = State $ \s ->
    let (a', s') = x s
     in runState (f a') s'

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Functor (State s) where
  fmap = liftM

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  (StateT x) >>= f = StateT $ \s -> do
    (a', s') <- x s
    runStateT (f a') s'

instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (StateT s m) where
  fmap = liftM

instance MonadTrans (StateT s) where
  lift c = StateT $ \s -> c >>= (\x -> return (x, s))
