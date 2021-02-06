module Data.Hashable where

import Prelude

import Control.Monad.Gen (elements)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.String.CodeUnits as String

data Bool = True | False

instance showBoolean :: Show Bool where
  show True = "True"
  show False = "false"

data List a = Cons a (List a) | End

-- pure script instance has name for some reason.
instance functorList :: Functor List where
  map _ End = End
  map f (Cons a b) = Cons (f a) (map f b)

-- multi paramter type classes
-- this will need multiParamTypeClasses in haskell.

-- functional dependencies
-- specify known stream implies known element
-- this helps the compiler to infer the property type
-- even if element is not present at all.
class Stream stream element | stream -> element where
    uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
    uncons = Array.uncons

instance streamString :: Stream String Char where
    uncons = String.uncons

foldStream :: forall l e m. (Stream l e) => Monoid m => (e -> m) -> l -> m
foldStream f list =
    case uncons list of
         Nothing -> mempty
         Just cons -> f cons.head <> foldStream f cons.tail


