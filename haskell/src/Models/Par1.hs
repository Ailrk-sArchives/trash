{-# LANGUAGE BangPatterns #-}
module Models.Par1 where

import           Control.DeepSeq
import           Control.Parallel
import           Control.Parallel.Strategies

import           Criterion                   as C
import           Criterion.Main              as CM

import           Data.Foldable               (foldl')

{-@ A normal seq will only evaluate data to their weak head normal form.
    deepseq evaluate it all the way to normal form.
    In parallel programming it's important to ensure pending work doesn't
    migrate to the wrong thread.
@-}


{-@ First style of controlling parallelization is with Eval monad and strategies.
@-}

sum' :: Foldable m => Num a => m a -> a
sum' = foldl' (+) 0

--   return here
-- sum x |--------------
-- sum y |--------
-- return immediately, doesn't wait either f x or f y.
run1 :: () -> (Int, Int)
run1 _ =  runEval $ do
  a <- rpar (sum' x)
  b <- rpar (sum' y)
  return (a, b)
  where
    x = [1..90000000]
    y = [1..10000]

-- This is a contrived example. For a real task, how do you know
-- task b finished faster than task a?
-- sum x -------|------
-- sum y -------|
-- return after y is done.
run2 :: () -> (Int, Int)
run2 _ = runEval $ do
  a <- rpar (sum' x)
  b <- rseq (sum' y)
  return (a, b)
  where
    x = [1..90000000]
    y = [1..10000]

-- rseq rseq make sure return after all tasks are finished.
-- this essentially put a barrier on all tasks.
-- sum x --------------|
-- sum y --------      |
-- return after y is done.
run3 :: () -> (Int, Int)
run3 _ = runEval $ do
  a <- rpar (sum' x)
  b <- rpar (sum' y)
  rseq a
  rseq b
  return (a, b)
  where
    x = [1..90000000]
    y = [1..10000]


-- turn into () to force recaluation.
run123 = CM.defaultMain [ C.bench "rpar: " $ C.nf  run1 ()
                        , C.bench "rseq: " $ C.nf run2 ()
                        , C.bench "rseqall: " $  C.nf run3 ()
                        ]
