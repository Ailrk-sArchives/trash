{-# LANGUAGE DeriveFunctor #-}

module EvalMonad where

-- System.IO has file io apis, System.Environment has args apis.
-- Control.Parallel provides Eval monad. Things like rpar, rseq etc.

import Data.Maybe
import Control.DeepSeq
import Control.Parallel.Strategies
import Sudoku
import System.IO

-- Lazy IO stuffs.

-- IO action is carried out by the outer driver that tries to execute all the
-- effects of main; The correct order of execution is maintained by IO's
-- monadic sequencing

-- A catch on withFile, the file will be release after the function
-- is exited, but because haskell is lazily evaluated the content of the
-- file will not be read until it's needed. If your computation entails the
-- file is forced by actions outside the handle function, withFile will close
-- the file before it's content even be read.

-- Another catch with hGetContents.
-- hGetContents doesn't evaluate anything, it will represent a thunk
-- for some other operations to use it.
-- Some ppl suggest to call it listen, this might capture more characteristics
-- of it.

-- To solve this we need to make sure s is fully evaluated before it is
-- returned. DeepSeq can help you do that.
-- seq or bang pattern is not enough because they only force evaluation to
-- the WHNF, stuffs remained still unevaluated.
puzzles :: IO [String]
puzzles = do
  r <- withFile path ReadMode $ \handle -> do
    s <- hGetContents handle
    return $!! s
  return $ lines r
  where
    path = "sudoku17.1000.txt"

-- control synchrnoization with rpar and rseq.

-- in sequential
run0 :: IO ()
run0 = do
  p <- puzzles
  let r = solve <$> p
  print $ length $ filter isJust r

-- split the work into two sub tasks.
-- join both
-- force will fully evalutae it's argument and then return it.
run1 :: IO ()
run1 = do
  p <- puzzles
  let (as, bs) = splitAt (length p `div` 2) p
      solutions = runEval $ do
        as' <- rpar $ force $ map solve as
        bs' <- rpar $ force $ map solve bs
        rseq as'
        rseq bs'
        return $ as' ++ bs'
  print $ length $ filter isJust solutions
