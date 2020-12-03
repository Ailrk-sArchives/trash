{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module ErrorHandling where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans


-- Monad transformers are literately a wrapper of any monad
-- over your target type.
-- Today it's ExceptT, tomorrow you can make MaybeT, after
-- you might have IMGuiT, ParserT, ConvertToCPPT or whatever you
-- want it.
newtype ExceptT e m a = ExceptT (m (Either e a))

-- basically runExcept just pull the value after the
-- data constructor out.
-- You wan to work with the underlying monad, ExceptT
-- is just a wrapper to hold all pieces together.
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT m) = m

-- map to an ExceptT means map two layer down into
-- it's inner value.
-- There're actually three layers, the outmost layer is
-- ExceptT itself, but it doesn't carry many useful information.
--
-- One layer inside is the monad wrapper, and that's what
-- you really care about.
-- Because the only constraint for this layer is just being a
-- monad, you can shove in whatever monad you like. Of course,
-- another monad transformer.
-- In this way, you can access different monads all together.
--
-- The innder most is your target type. We have Either here
--
-- fmap a function into a monad transformer means lift over
-- all this three layers.
instance (Monad m) => Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT ((fmap . fmap) f m)

instance (Monad m) => Applicative (ExceptT e m) where
  pure = return
  (<*>)  = ap

-- look at the return you can see three layers clearly.
-- for >>=, be aware after ExceptT we are in the m monad.
instance (Monad m) => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  m >>= k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left e  -> return . Left $ e
      Right a -> runExceptT (k a)


-- construct an exceptT with left value.
-- this creates an exception that can be handled easily.
throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

-- carry out inner computation, if an exception throws
-- handle it with the handler
catchE :: (Monad m)
       => ExceptT e m a
       -> (e -> ExceptT e' m a)
       -> ExceptT e' m a

m `catchE` h = ExceptT $ do
  a <- runExceptT m
  case a of
    Left l  ->  runExceptT (h l)
    -- of course if it's right you don't care about e' anymore.
    Right r -> return (Right r)


-- MonadTrans for ExceptT
-- What does lift do is essentially lift a monadic
-- value into the ExcepT
-- There are two things you need to ensure:
--  1. the result is a ExceptT.
--  2. the inner value `a` of `m a` should be lifted to `Either e a`
--  It looks lie this.
--  monad a -> ExceptT | monad | Either e a
instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right


-- This is mtl style monad transformer.
-- What does it do?
-- Essentially it helps you to lift the underlying
-- monad up no matter how many layers it is buried in.
--
-- m can be any monad, as long as we define this
-- typeclass for several monads. If we combine this
-- monads together we can deverive a working MonadError
-- which brings error up even if it is arbitrarily
-- deep in the monad stack.
--
-- The similar idea also apply to MonadState, MonadReader etc.
class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError IOException IO where
  throwError = ioError
  catchError = catch


instance MonadError e (Either e) where
  throwError = Left
  Left l `catchError` h  = h l
  Right r `catchError` _ = Right r
