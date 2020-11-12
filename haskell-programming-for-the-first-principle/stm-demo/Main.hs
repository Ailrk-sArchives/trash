{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Parallel.Strategies
import           Data.Digest.Pure.MD5        (md5)

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString.Lazy        as L
import           System.Environment


main :: IO ()
main = Prelude.putStrLn "Hello, Haskell!"

{- sparks
    - Most basic atomic parallelism.
    - A hint to GHC runtime that a computation can be evaluated
      to WHNF in parallel

    For example, `rpar a`:
      - spin a separate spark
      - evaluate to WHNF
      - place the computation in the spark pool.

    Terms:
      * Fizzled: The result has already been evaluated by the main
                 thread so the spark need not to be converted
      * Dud: The expression has already been evaluated, the computed value
             the computed value is returned and the spark is not converted.
      * CG'd: The spark is added to spark pool but the rseult it not referenced,
              so the result is collected.
      * Ovverflowed: Insufficient space in the spark pool when spanwing.

    Note:
      Parallel runtime is necessary for using spark, with GHC, -threaded must be
      turned on.
-}


sparkEx :: (a -> b) -> a -> a -> (b, b)
sparkEx f x y = runEval $ do
  a <- rpar $ f x
  b <- rpar $ f y
  rseq a          -- make sure after result is present after the call.
  rseq b
  return (a, b)


{- Thread.
    In haskell we use green threads. There are bound threads (OS threads) and unbound threads.
    We can choose to run a computation on bound thread or unbound thread with
      runInBoundThread and runInUnboundThread
    Threads live in IO monad.
-}





{- Chan

-}


------------------------------------------------------------------------------
-- Some examples.
------------------------------------------------------------------------------

-- Use MVar to control threads.
{-
   Locking mutable variable (MVar)
      - common for communicating values
      - used for signaling

  the function `takeMVar :: Mvar a -> IO a` will block until the MVar is non-empty
  `putMVar :: MVar a -> a -> IO ()` will block until the current MVar is empty.


-}
runWithMVar :: IO ()
runWithMVar = do
  files <- getArgs
  str <- newEmptyMVar     -- create a new MVar to protect file content.
  mapM_ (forkIO . hashAndPrint str) files
  presults (Prelude.length files) str
 where
   presults i var = replicateM_ i (takeMVar var >>= Prelude.putStrLn)

-- here the content of the file is protected by MVar.
hashAndPrint :: MVar String -> FilePath -> IO ()
hashAndPrint str f = do
  bs <- L.readFile f
  let !h = show $ md5 bs  -- get strictly evaluated here.
  putMVar str (f ++ ": " ++ h)
