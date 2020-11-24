{-# LANGUAGE MultiWayIf #-}
module Conduits where

import           Control.Monad.Trans
import           Data.Conduit
import qualified Data.Conduit.List   as CL

{-@ Conduit
    Conduit provides stream like mechanism, so you can load infinite
    data without load all of them into the memory at once.

    Although haskell already has lazy list, but you can't directly
    perform IO on top of it.

    Or maybe you can use lazy IO, but that's proven causes too many
    suprises. The most well known example is hWithFile.

    Conduit solve both problems above.

    ConduitT i o m r
      where  r is the result.
        i, o represent two end of the conduit
        m is for monad transformer.
@-}

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
conduit :: ConduitT Int String IO ()
conduit = do
  val <- await
  case val of
    Nothing -> return ()
    Just n -> do
      if | n `mod` 15 == 0 -> yield "FizzBuzz"
         | n `mod` 5 == 0  -> yield "Fizz"
         | n `mod` 3 == 0  -> yield "Buzz"
         | otherwise       -> return ()
      conduit


sink :: ConduitT String o IO ()
sink = CL.mapM_ putStrLn

-- two end of the conduit are called source and sink.
-- the previous conduit is called upstream, next conduit is called
-- downstream.
run :: IO ()
run = runConduit $ source .| conduit .| sink
