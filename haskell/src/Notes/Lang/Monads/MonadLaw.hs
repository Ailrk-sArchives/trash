module Notes.Lang.Monads.MonadLaw where

import Control.Applicative
import Control.Monad

{- HLINT ignore -}
-- source no where.
-- just a monad that check if the monadic rule wholes.

-- instance Functor MonadCheck where
--   fmap f (MonadCheck m) = MonadCheck $ f m
rightUnitLaw :: (Monad m, Eq (m a)) => m a -> Bool
rightUnitLaw m = (m >>= return) == m

leftUnitLaw :: (Monad m, Eq (m a)) => a -> (a -> m a) -> Bool
leftUnitLaw x f = (return x >>= f) == f x

associativityLaw :: (Monad m, Eq (m a)) => m a -> (a -> m a) -> (a -> m a) -> Bool
associativityLaw m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

bind :: (Monad m) => m a -> (a -> m b) -> m b
bind m g = join (fmap g m)

join1 x = x >>= id
