{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Libs.Conduits where

import           Control.Lens
import           System.Process

import           Conduit
import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import           Control.Monad.Trans
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import qualified Data.Conduit.Process     as CP
import           Data.Functor.Identity

import qualified Data.ByteString          as B

{-@ Why using conduit if we already have lazyness?

    - Laziness doesn't allow you to perform IO inbetween each chunks.
    - Lazy IO is problematic.
      - Side effect in the middle of the pure code. (possibility of Exception in IO)
      - Nondeterministic resource handling

    Essentially laziness is not a feature that works well with side effect.
    But in some senarios that we need stream data, perform IO between chunks
    is very important. Conduit makes the work a bit easier.
@-}

{-@ Conduit
    Conduit provides stream like mechanism, so you can load infinite
    data without load all of them into the memory at once.

    Although haskell already has lazy list, but you can't directly
    perform IO on top of it.

    Or maybe you can use lazy IO, but that's proven causes too many
    suprises. The most well known example is hWithFile.

    Conduit solve both problems above.

    ConduitT i o m r
      where
        r is the result.
        i, o represent two end of the conduit
        m is for monad transformer.
@-}

{-@ simple example: @-}

-- Explain:
-- A conduit with no input data type. This implies it's the source of
-- the entire conduit.
-- The conduit has no result, so it's not the end of the conduit.
-- This conduit will yield Int to the next conduit connect to it.
-- You can perform IO operation in this conduit Monad.
source :: ConduitT () Int IO ()
source = CL.sourceList [1..100]

-- Notice it's a recursive function and doesn't halt.
-- However the entire conduit still halt eventually.
-- Because when does the conduit halt only depends on the source.
-- This conduit is merely a component perform some logic.
conduitFizzBuzz :: ConduitT Int String IO ()
conduitFizzBuzz = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      if | n `mod` 15 == 0 -> yield "FizzBuzz"
         | n `mod` 5 == 0  -> yield "Fizz"
         | n `mod` 3 == 0  -> yield "Buzz"
         | otherwise       -> return ()
      conduitFizzBuzz

conduitAlertIfBuzz :: ConduitT String String IO ()
conduitAlertIfBuzz = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      when (n == "Buzz") $ liftIO (putStrLn "== I found Buzz!! ==")  -- lift effect in the middle)
      yield n
      conduitAlertIfBuzz

conduitToUpper :: ConduitT String String IO () -- add a ne conduit
conduitToUpper = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      yield $ toUpper <$> n
      conduitToUpper

conduitQuoteInTag :: ConduitT String String IO ()
conduitQuoteInTag = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
       yield $ "<" ++ n ++ ">"
       conduitQuoteInTag

sink :: ConduitT String o IO ()
sink = CL.mapM_ putStrLn

-- two end of the conduit are called source and sink.
-- the previous conduit is called upstream, next conduit is called
-- downstream.
run :: IO ()
run = runConduit $ source
    .| conduitFizzBuzz
    .| conduitAlertIfBuzz
    .| conduitToUpper
    .| conduitQuoteInTag
    .| sink


{-@ Conduit 2 @-}

source1 :: ConduitT () Int IO ()
source1 = CL.sourceList [1..100]

conduit1 :: ConduitT Int String IO ()
conduit1 = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      liftIO (putStrLn (show n))
      yield (show n)
      conduit1

sink1 :: ConduitT String o IO ()
sink1 = CL.mapM_ putStrLn

run1 :: IO ()
run1 = runConduit $ source1 .| conduit1 .| sink1


{-@ Conduit @-}
run2 = yieldMany [1..10] .| sumC
     & runConduitPure
     & print
