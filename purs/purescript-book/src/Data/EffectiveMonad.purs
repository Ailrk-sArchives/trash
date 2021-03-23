module Data.EffectiveMonad where

import Data.Eq

import Control.Alternative (empty)
import Control.Applicative ((<*>))
import Control.Monad (ap)
import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref (new, modify, read)
import Data.Array (foldM, nub, sort, (..))
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.List (List(..), fromFoldable, head, tail, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Effect (Effect)
import Effect.Aff (error)
import Effect.Class.Console (log, logShow)
import Effect.Exception (message, throwException, try)
import Effect.Random (random)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude (class Monad, Unit, bind, pure, (&&), (+), (/), (<<<), (==), (>>=), ($), (*), (-), discard)


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
foldM' f a (b:bs) = f a b >>= \a' -> foldM' f a' bs

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

-- exampe of using it.
chainDiv :: Int -> Array Int -> Maybe Int
chainDiv divident = foldM' safeDiv divident <<< fromFoldable

-- monad introduce data dependencies in a chain of computation
-- while applicative don't

-- exercise
third :: forall a. List a -> Maybe a
third list = do
    t' <- tail list
    t'' <- tail t'
    h' <- head t''
    pure h'

sums :: Array Int -> Array Int
sums = nub <<< sort <<< foldM (\acc a -> [acc, acc + a]) 0

check :: forall a. Eq a => Maybe a -> Boolean
check a = ((Just (_+1) <*> Just 2) == Just (_+1) `ap` Just 2)
       && ((Just (_+1) <*> Nothing) == Just (_+1) `ap` Nothing)

filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM' f Nil = pure Nil
filterM' f (x:xs) = f x >>= \b ->
    if b then (x:_) <$> filterM' f xs else filterM' f xs

-- every monad has a functor instance.
mapMonad :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
mapMonad f a = do
    x <- a
    pure (f x)

-- Native Effects.
-- some exampes: console IO, random number generation, exception rw mut state.
--               DOM, XMLHttpRequest, websocket, rw local storage.

main1 :: Effect Unit
main1 = do
    n <- random
    logShow n

-- handle exception gracefully.
main2 :: Effect Unit
main2 = do
    result <- try $ readTextFile UTF8 "iDoNotExist.md"
    case result of
         Right lines -> log $ "Content: \n" <> lines
         Left error ->  log $ "Couldn't open file. Error: " <> message error

-- haskell style head, throw exceptions directly.
exceptionHead :: List Int -> Effect Int
exceptionHead l = case l of
    x : _ -> pure x
    Nil -> throwException $ error "empty list"

-- mutable state
-- ST effect. restrict sharing mutatble state in such a way that
-- only safe local mutation is allowed.
--
-- simulate the movement of a particle falling under gravity
-- ST r a = ST r a
-- a is value stored in the cell.
-- r is called a Region kind, which the compiler will make sure
-- the reference cell is not going to escape its scope. and
-- can safely turn ref into value.
--
-- new :: forall a r. a -> ST r (STRef r a)
-- read :: forall a r. STRef r a -> ST r a
-- write :: forall a r. a -> STRef r a -> ST r a
-- modify :: forall r a. (a -> a) -> STRef r a -> ST r a

simulate :: Number -> Number -> Int -> Number
simulate x0 v0 time = run do
    ref' <- new { x: x0, v: v0 }
    for 0 (time * 1000) \_ ->
        modify ( \o ->
            { v: o.v - 9.81 * 0.001
            , x: o.x + o.v * 0.001 })
            ref'
    final <- read ref'
    pure final.x


