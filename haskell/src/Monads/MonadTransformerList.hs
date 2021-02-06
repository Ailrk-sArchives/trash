{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Monads.MonadTransformerList where

import           Control.Applicative
import           Control.Exception   (IOException (..), catch, ioError)
import           Control.Monad
import           Control.Monad.Trans (MonadTrans (..))

{-@ Recap, let's make some monad transformers
    First is the maybe monad.
 @-}
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f =  MaybeT $ do
    value <- runMaybeT x
    case value of
      Just value' -> runMaybeT $ f value'
      Nothing     -> return Nothing

instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do
    x_val <- runMaybeT x
    case x_val of
      Nothing -> runMaybeT y
      Just _  -> return x_val

{-@ Then let's make some exceptions
    This time we can also practice some mtl style interface
 @-}
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance (Monad m) => Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT ((fmap . fmap) f m)

instance (Monad m) => Applicative (ExceptT e m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  m >>= f = ExceptT $ do
    val <- runExceptT m
    case val of
      Left e  -> return . Left $ e
      Right a -> runExceptT $ f a

-- so far we just get the infrastructure of ExceptT, we can merely
-- say it's a monad. Bit other than being monad, we want a paif of
-- operator like throw and catch so we can throw an error somewhere
-- in the code and signal the caller who knows how to handle it.

throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: (Monad m) => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` e = ExceptT $ do
  val <- runExceptT m
  case val of
    Left e' -> runExceptT (e e')
    Right a -> return . return $ a

instance MonadTrans (ExceptT e) where
  lift = ExceptT . (fmap Right)

-- the idea of a mtl interface is to parameterize all specific types
-- and provide an unique interface.
-- m can be Either e a, ExceptT e a, or anything else.
class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- let's write some MonadError interface for conveneince.

instance MonadError e (Either e) where
  throwError = Left
  Left l `catchError` h  = h l
  Right r `catchError` _ = Right r

instance MonadError IOException IO where
  throwError = ioError
  catchError = catch

{-@ I always though monad transformer sorta forms a
    list. If that's the case, it should be able to be
    expressed as type level list.
@-}


