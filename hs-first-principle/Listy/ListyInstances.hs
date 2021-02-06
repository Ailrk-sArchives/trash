module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
    mempty = Listy []

instance Semigroup (Listy a) where
    Listy [] <> Listy [] = Listy []
    Listy a <> Listy [] = Listy a
    Listy [] <> Listy a = Listy a
    Listy a <> Listy a' = Listy (a <> a')


