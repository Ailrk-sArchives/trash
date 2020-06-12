module Data.Hashable where

import Prelude

import Control.Monad (ap)


data Bool = True | False

instance showBoolean :: Show Bool where
  show True = "True"
  show False = "false"

data List a = Cons a (List a) | End

-- pure script instance has name for some reason.
instance functorList :: Functor List where
  map _ End = End
  map f (Cons a b) = Cons (f a) (map f b)


