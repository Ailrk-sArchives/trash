module QuickSort where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans
import Data.Array.ST
import Data.Array
import Data.Array.Unsafe
import System.Random

-- slow quick sort
-- this will copy memory for each upate
quicksortSlow :: (Ord a) => [a] -> [a]
quicksortSlow [] = []
quicksortSlow (x : xs) = left ++ [x] ++ right
  where
    left = quicksortSlow $ filter (<= x) xs
    right = quicksortSlow $ filter (> x) xs

-- in place quick sort with ST monad
swap :: STArray s Int a -> Int -> Int -> ST s ()
swap arr i j = do
  a <- readArray arr i
  b <- readArray arr j
  writeArray arr i b
  writeArray arr j a

partition' :: (Ord a) => STArray s Int a -> a -> Int -> StateT Int (ST s) ()
partition' arr pivot i = do
  pivotIdx <- get
  this <- lift $ readArray arr i
  when (this < pivot) $ do
    lift $ swap arr i pivotIdx
    put (pivotIdx + 1)

-- partition loop
partition ::
  (Ord a) => STArray s Int a -> Int -> Int -> ST s Int
partition arr start end = do
  pivot <- readArray arr start
  let pivotIdx0 = start + 1
  finalPivotIdx <-
    execStateT
      (mapM (partition' arr pivot) [(start + 1) .. (end - 1)])
      pivotIdx0
  swap arr start (finalPivotIdx - 1)
  return $ finalPivotIdx - 1

-- end the ST effect here.
quicksortHelper :: (Ord a) => Int -> Int -> STArray s Int a -> ST s ()
quicksortHelper start end stArr = when (start + 1 < end) $ do
  pivotIdx <- partition stArr start end
  quicksortHelper start pivotIdx stArr
  quicksortHelper (pivotIdx + 1) end stArr

-- provides a pure interface
quicksort :: (Ord a) => Array Int a -> Array Int a
quicksort arr = runSTArray $ do
  stArr <- thaw arr
  let (min, max) = bounds arr
  quicksortHelper min (max + 1) stArr
  return stArr

quicksort1 arr = runSTArray


-- a randome array for testing.
randomArray :: IO (Array Int Int)
randomArray = do
  randoms <- replicateM 100 $ randomRIO (1, 1000)
  return $ array (1, 100) (zip [1 .. 100] randoms)
