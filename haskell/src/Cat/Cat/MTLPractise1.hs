{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cat.Cat.MTLPractise1 where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

type UserInput = String

{-@ Had some problem to derive both readerT and exceptT
    Some experiments.
@-}

-- Derive one
newtype Foo a = Foo {unFoo :: ReaderT UserInput IO a}
  deriving (Functor, Applicative, Monad, MonadReader UserInput)

foo :: Foo String
foo = do
  res <- ask
  case res of
    (x : xs) -> return ("stitch with: " ++ xs)
    _ -> return "no"

tryfoo = runReaderT (unFoo foo) "tthis"

newtype Bar a = Bar {unBar :: ExceptT String IO a}
  deriving (Functor, Applicative, Monad, MonadError String)

bar :: Bar String
bar = throwError "err"

trybar = runExceptT (unBar bar)

-- now try to derive two together

newtype Woo a = Woo {unWoo :: ReaderT String (WriterT String IO) a}
  deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String, MonadIO)

woo :: Woo String
woo = do
  val <- ask
  tell ("written: " ++ val)
  return val

trywoo = runWriterT (runReaderT (unWoo woo) "read")

newtype Woo' a = Woo' {unWoo' :: ExceptT String (ReaderT String IO) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader String)
