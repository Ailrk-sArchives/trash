module Data.EffectiveMonad where

import Control.Alternative (empty)
import Data.Array ((..))
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..))
import Prelude (class Monad, bind, pure, (+), (/), (<<<), (==), (>>=))


countThrows :: Int -> Array (Array Int)
countThrows n = do
    x <- 1 .. 6
    y <- 1 .. 6
    if x + y == n
        then pure [x, y]
        else empty

-- in pure script Monad bind >>= is further separated into
-- Bind typeclass.
-- monad is defined as class (Applicative m, Bind m) <= Monad m
-- where class Apply m <= Bind m


-- foldM generalize foldl
-- perform a fold over a list in some context supporting some set
-- of side-effects.
-- for example, if m is Maybe, then the operation can fail at any
-- stage of the computation.
--
-- for Array, every step of the fold would be allowed to return
-- zero or more results, and the fold would proceed to the next step
-- independently for each result. At the end, the set of results would
-- consist of all folds over all possible paths, which corresponds to a
-- traversal of a graph.
foldM' :: forall m a b
    .  (Monad m)
   => (a -> b -> m a)
   -> a
   -> List b
   -> m a
foldM' _ a Nil = pure a
foldM' f a (b:bs) =  f a b >>= \a' -> foldM' f a' bs

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

-- exampe of using it.
chainDiv :: Int -> Array Int -> Maybe Int
chainDiv divident = foldM' safeDiv divident <<< fromFoldable

-- monad introduce data dependencies in a chain of computation
-- while applicative don't

