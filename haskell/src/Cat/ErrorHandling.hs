{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cat.ErrorHandling where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Writer
import           Data.Char                      as Char
import           Data.List

import Control.Monad.Cont

-- helper name. This is how it's defined in mtl.
-- How to catch an exception?
-- catch an exception means takes a monad that may throws an exception, and a
-- function that can turn an exception back into a ok value.
-- return the same (m a) if no error, other return the return value of (e -> m a)
type Catch e m a = m a -> (e -> m a) -> m a

-- what is an exception? exception is something that either be an intended value or
-- an exceptional value.
--
-- what is an exceptT? ExceptT is some monadic computation that return eitehr a intended
-- value or an exceptional value.
--
-- what is an exceptional value? It's a value that is not expected in a successful execution path.
-- we can use it to throw useful information of how the exception happened.
newtype ExceptT e (m :: * -> *) a = ExceptT {runExceptT :: m (Either e a)}

instance Monad m => Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT ((fmap . fmap) f m)

instance Monad m => Applicative (ExceptT e m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  m >>= k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left e  -> (return . Left) e
      Right a -> runExceptT (k a)

-- what is a monad transformer instance for ExceptT e ?
-- Monad transformer instance for  ExceptT e is a monad that supports lift.
--
-- what is lift?
-- lift is a funtion that lift a monadic computation to ExceptT so it can be
-- performed at the same level.
--
-- what does it mean to perform monadic computation at the same level?
-- it means propagate the value of the result of the underlying computation.
-- In this case, the underlying monad will produce a value, to bring it up to
-- ExceptT means wrap it in ExceptT . Right.
--
-- lift a monad by one layer into ExceptT
instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right

-- monad transfromer is explained in Eh.hs
instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

-- with these ingradients we can simulate try catch.
-- By defualt, lift propagate Right.
-- But what if we want to propagate an error?
--
-- We need another function to do that. This function is essentially the same as lift, just
-- return a Left value instead of Right.
throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left -- return but for Left value.

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = ExceptT $ do
  a <- runExceptT m
  case a of
    Left e  -> runExceptT (h e)
    Right r -> return (Right r)

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- use mtl to generalize some existed error handling monads
instance MonadError IOException IO where
  throwError = ioError
  catchError = catch

instance MonadError e (Either e) where
  throwError = Left
  Left e `catchError` h  = h e
  Right r `catchError` _ = Right r

instance Monad m => MonadError e (ExceptT e m) where
  throwError = throwE
  catchError = catchE

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

-- To make MonadError work with another monad transformer,
-- we need to make an instance for that transformer as well.
--
-- To make MonadError work with 2 other monads transformer,
-- we need to make 2 instances for those transformers .
-- this is the n square instance problem.

liftCatchReaderT :: Catch e m a -> Catch e (ReaderT r m) a
liftCatchReaderT f m h =
  ReaderT $ \r -> f (runReaderT m r) (\e -> runReaderT (h e) r)

-- this requires undecidable instance
instance MonadError e m => MonadError e (ReaderT r m) where
  throwError = lift . throwError
  catchError = undefined

-- examples
-- look at the type, you see:
-- 1. this function is possible to fail.
-- 2. you can use throwError to Indicate a failure
-- 3. The caller can call catchError with a handler to handle
--    the faliure case.

-- test 1 --
throwMe1 :: Int -> Either String Int
throwMe1 n
  | n > 100 = Left "Too big"
  | n < 0 = Left "too small"
  | otherwise = Right n

catchMe1 :: MonadError e m => Int -> (Int -> m Int) -> m Int
catchMe1 n f = catchError (f n) (\_ -> return (-1))

-- testing Eitehr
try1 = catchMe1 10 throwMe1
try2 = catchMe1 (-1) throwMe1
try3 = catchMe1 (999) throwMe1

-- test 2 --
throwMe2 :: Int -> ExceptT String IO Int
throwMe2 n = do
  liftIO (putStr "It's a transformer stack with except on top of IO")
  go n
  where
    go :: Int -> ExceptT String IO Int
    go n
      | n > 100 = throwError "Too big"
      | n < 0 = throwError "too small"
      | otherwise = return n

catchMe2 :: (MonadError e m, MonadIO m) => Int -> (Int -> m Int) -> m Int
catchMe2 n f =
  catchError
    (f n)
    ( \e -> do
        liftIO (putStr "Error occured")
        return (-1)
    )

-- testing ExceptT
try4 = runExceptT $ catchMe2 10 throwMe2
try5 = runExceptT $ catchMe2 (-1) throwMe2
try6 = runExceptT $ catchMe2 111 throwMe2

-- some bigger example to combine multiple mondas together.
-- validate input email --

data NameError
  = EmptyName
  | InvalidChar String
  | NameTooLong Int
  | BannedWords
  deriving (Show)

-- | check if email is valid
-- for our purpose, the valid form should be "[\w\d]+@\.com]"
validateFormat :: (MonadError NameError m) => String -> m String
validateFormat input =
  if fst (runState check input)
    then return input
    else throwError (InvalidChar input)
  where
    isValidChar = Char.isAlphaNum
    check :: State String Bool
    check = do
      input <- get
      case input of
        (x : xs) ->
          if isValidChar x
            then return (evalState check xs)
            else return False
        [] -> return True

validateLength :: (MonadError NameError m) => String -> m String
validateLength input
  | length input > 30 = throwError (NameTooLong (length input))
  | length input == 0 = throwError (EmptyName)
  | otherwise = return input

validateBandedWords :: (MonadError String m) => String -> m String
validateBandedWords input
  | and [w `elem` input' | w <- banned] = throwError ""
  | otherwise = return input
  where
    input' = words input
    banned = ["peepee", "poopoo", "woowoo"]


-------------------------------------------------------------------------------
-- We can also use cps to simulate exceptions

-- c takes an error handling funcion and only call it when exception happen.
tryit :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryit c h = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    x <- c notOk
    ok x
  h err

data SqrtException = LessThenZero deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
  ln <- lift (putStr "Enter a number to sqrt: " >> readLn)
  when (ln < 0) (throw LessThenZero)
  lift $ print (sqrt ln)

-- this is very interesting
runSqrtIO = runContT (tryit sqrtIO (lift . print)) return
