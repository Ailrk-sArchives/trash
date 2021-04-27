{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Monads.ErrorHandling where

import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Writer
import Data.Char as Char
import Data.List

-- helper name
type Catch e m a = m a -> (e -> m a) -> m a

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
      Left e -> (return . Left) e
      Right a -> runExceptT (k a)

-- lift a monad by one layer into ExceptT
instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right

-- if it's IO, liftIO is id.
-- otherwise keep lifting the IO type until hit IO (base case).
-- this is a recursion in a global scope.
instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

-- with these ingradients we can simulate try catch.
throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left -- return but for Left value.

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = ExceptT $ do
  a <- runExceptT m
  case a of
    Left e -> runExceptT (h e)
    Right r -> return (Right r)

-- generalize the idea of throwing and catching exceptions.
-- we make it a mtl style type class.

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- use mtl to generalize some existed error handling monads

instance MonadError IOException IO where
  throwError = ioError
  catchError = catch

instance MonadError e (Either e) where
  throwError = Left
  Left e `catchError` h = h e
  Right r `catchError` _ = Right r

instance Monad m => MonadError e (ExceptT e m) where
  throwError = throwE
  catchError = catchE

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

-- not to make MonadError work with other monad transformer,
-- we need to make an instance for that transformer as well...
-- this is the n square instance problem.

liftCatchReaderT :: Catch e m a -> Catch e (ReaderT r m) a
liftCatchReaderT f m h =
  ReaderT $ \r -> f (runReaderT m r) (\e -> runReaderT (h e) r)

-- this requires undecidable instance
instance MonadError e m => MonadError e (ReaderT r m) where
  throwError = lift . throwError
  catchError = undefined

-- examples
-- Once you look at the type, you realize several things:
-- 1. this function is possible to fail.
-- 2. you can use throwError to Indicate a faildure
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

-- ok now try our combination

-- newtype NameValidator a = NameValidator
--   { runNameValidator :: ExceptT NameError (ReaderT UserInput IO) a
--   }
--   deriving newtype
--     ( Functor,
--       Applicative,
--       Monad,
--       MonadIO,
--       MonadError NameError,
--       MonadReader UserInput
--     )

-- validateName :: String -> NameValidator String
-- validateName name = do
--   input <- ask
--   return "asd"
