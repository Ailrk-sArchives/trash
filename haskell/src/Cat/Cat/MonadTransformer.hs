{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Cat.Cat.MonadTransformer where

import           Control.Applicative
import qualified Control.Exception      as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans

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

instance MonadTrans MaybeT where
  lift = MaybeT . (fmap Just)


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

-- Monad transformer.
-- 1. general form is:
--    Monad m => Monad e => m (f a)
--    m is the wrapper monad, f is the base monad.
--
--    base monad means the monad that determine the functionality
--    of the monad.
--
--    wrapper monad means whatever monad that stack on top of it.
--
-- 2. to hold pieces together we use newtype to wrap
--    stuffs into a new type (ha).
--
--    Some code are just noise to deal with new type itself.
--
-- 3. it really works like linked list
--
--    We know different monads have different semantics. Dealing with
--    a single monad is simple, we can just work with it alone.
--    But what about multiple monads
--    Say Monad1 Monad2, Monad3, Monad4 all doing different things. We
--    want a mega monad that can do all of them.
--
--    (In non pure languages you don't need to stack anything, the whole
--    program is a gigantic monad that works for everything.
--      pros: easier to work with
--      cons: can't split side effects, so can't be pure.
--            effects not in type, less explicit on what effects to perform.
--    )
--
--    newtype MonadTransformer1 m a = MonadTransformer1 (m (Monad1 a))
--    newtype MonadTransformer2 m a = MonadTransformer2 (m (Monad2 a))
--    newtype MonadTransformer3 m a = MonadTransformer3 (m (Monad3 a))
--
--    type MegaMonad' a
--      =  MonadTransformer1 (
--         MonadTransformer2 (
--         MonadTransformer3 (
--           Monad4 a
--         )))
--
--   To be honest I hate the syntax. the nested structure is hard to deal with.
--   Also monads don't compose well so sometimes the order matters. (IO->Except/Except->IO)
--
--
--   Anyway. we have the representation of the mega monad. The next thing to do is to
--   hide the internal make it works as a solid piece. Again we use newtype.
--
--   newtype MegaMonad a = MegaMonad { unMega :: MegaMonad' a }
--
--   then we can derive lots of typeclass to lift things for us automatically.

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

-- runExceptT exists only for newtype. If we don't have newtype wrapper
-- we don't need this.
-- It's not the core of the semantic of a monad transformer.

type EMIO a = ExceptT Bool (MaybeT IO) a
-- a value looks like: ExceptT (MaybeT (IO (Maybe (Either a))))
-- see how constructor and the actual moand is in reverse

-- See, lifting order follows the order of the transformers' constructors.
-- but actual moand follows the opposite order
workONEMIO :: EMIO Int
workONEMIO = do
  i1 <- catchE (throwE False) (\_ -> return 1)
  n <- lift $ return 10
  lift . lift $ putStrLn "asd"
  return 1

unwrapEMIO = runMaybeT . runExceptT

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


-- again, what's the semantic of an action that can have exception?
-- it can return a normal value, or return a exceptional value.
--
-- what do we want to do with exceptions?
-- When some conditions holds, we want to throw an exception to indicate something
-- wrong.
--
-- And somewhere at the call site we want to recognize the exception and handle it.
--
-- Thus we need a pair of operations:
--    throw to throw exception from nowhere
--    catch to handle exception and bring the program back to normal again.
throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: (Monad m) => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` e = ExceptT $ do
  val <- runExceptT m
  case val of
    Left e' -> runExceptT (e e')
    Right a -> return . return $ a

-- fmap a function into a monad transformer means lift over
-- all this three layers.
--
-- What is a functor?
-- A functor holds a context, and allows a function to apply to
-- value in the context.
--
-- What is a functor for ExceptT?
-- A functor for exceptT holds a context m that can be arbitray side effects,
-- which return a (Either e a).
--
-- This implies that we actually have two layers of monds.
-- the first one is m, the second one is (Either e)
--
-- so to map f into the inter most value, we need to lift it
-- twice.
--
-- And finally, ExceptT is a newtype, we have to do the newtype thing:
-- A dummy constructor.
instance MonadTrans (ExceptT e) where
  lift = ExceptT . (fmap Right)

-- generalize the idea of throwing and catching exceptions.
-- we make it a mtl style type class.

-- What is a MonadError type class?
-- a MonadError e m is a monad on m with exception of type e that
-- supports throwError and catchError.
--
-- What is throwError, what is catchError?
-- throwError takes an exceptional value, base on that return (m a) that
--  the carries exception informaotion.
--
-- catchError takes a monad, a function that handles exceptional value so
--  it can return a normal value, return a normal value.
--
-- Why you want this?
--  MonadError abstract away what monad can throw errors. Now any monad can
--  possibly throw an error.
--  Specifically, if we have a monad transfromer stack, it's a different monad
--  from any other monads defined in base.
--  It's possible that some layer in the stack can throw an error, but we need to
--  lift it.
--
--  Instead, if we implement this typeclass on top of the transformer, we can
--  1. indirectly call the underlyng throw and catch.
--  2. or we can do totally different thing, it doesn't matter since it's just
--     saying there exists overloads for this monad.
--
-- Why we need funciontal dependencies? avoid ambigious types.

class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- let's write some MonadError interface for conveneince.

instance MonadError e (Either e) where
  throwError = Left
  Left l `catchError` h  = h l
  Right r `catchError` _ = Right r

instance MonadError E.IOException IO where
  throwError = ioError
  catchError = E.catch

instance Monad m => MonadError e (ExceptT e m) where
  throwError = throwE
  catchError = catchE

instance (Monad m, MonadIO m) => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

{-@ Possible use case @-}

-- throw in the code (If there is an error wrap it in Left.)
throwMeEither:: Int -> Either String Int
throwMeEither n |  n > 100 = Right n
  | otherwise = Left "Bad bad"

throwMeExcept :: Int -> ExceptT String IO Int
throwMeExcept n | n > 100 = return n
  | otherwise = throwError "Bad bad except"

-- capture with an handler. With monad error we can't capture
-- Either and ExceptT with the same function.
catchMe :: MonadError e m => Int -> (Int -> m Int) -> m Int
catchMe n f = catchError (f n) (\_ -> return 1)

{-@ Some exampels @-}
-- define your own exception
data LengthError = EmptyString
                 | StringTooLong Int
                 | OtherError String

instance Show LengthError where
  show EmptyString = "the string is empty"
  show (StringTooLong len) = "The length of the string " ++ (show len) ++ " is too long"
  show (OtherError msg) = msg

newtype Length a = Length { unLengh :: ExceptT LengthError IO a }
  deriving (Functor, Applicative, Monad, MonadError LengthError, MonadIO)

-- note it's not necessary to catch every throw, since throw is really
-- just a pure for the error case anyway.
stringLengthExample  :: IO ()
stringLengthExample = do
  r <- runExceptT (unLengh calculateLength)
  report r

report :: Either LengthError Int -> IO ()
report (Right len) = putStrLn ("length of the str is: " <> show len)
report (Left e)    = putStrLn ("Error: " ++ show e)

calculateLength :: Length Int
calculateLength = do
  liftIO . putStrLn $ "Please enter a string"
  s <- liftIO getLine
  if null s
    then throwError EmptyString
    else let len = length s
          in if len > 50
                then throwError $ StringTooLong len
                else return len

-- catchMe 10 (\n -> if n == 10 then throwError "bad bad" else return 1)

{-@ quirks when catching IO @-}

-- this will crash.
-- First the thunk head [] is created, then it will live for the entire time until you
-- evaluate it, which in this case is never. Essentially no exception happend in the code,
-- the exception happend when ghci try to print the value out.
catchWithReturn = E.catch (return $ head []) $ \(_ :: E.SomeException) -> return "good"

-- This wont crash, because ghci doesn't need to print anything, the exceptional code (head [])
-- is still there like a bomb.
catchWithReturnUnit = E.catch (return $ head []) $ \(_ :: E.SomeException) -> return ()

-- evaluate force it's argument to be evaluated into weak head normal form.
-- this means (head []) get evaluated in place, and it get returned directly.
catchWithEvaluate = E.catch (E.evaluate $ head []) $ \(_ :: E.SomeException) -> return "good"
