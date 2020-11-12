{-# LANGUAGE DeriveFunctor #-}

module Cont where

import Control.Monad

newtype Cont r a = Cont {runCont :: (a -> r) -> r} deriving (Functor)

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return a = Cont ($ a)
  Cont s >>= f = Cont $ \k ->
    s $ \x ->
      runCont (f x) $ k

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h
