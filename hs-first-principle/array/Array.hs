module Array where

{-# OPTIONS_GHC -fglasgow-exts #-}

import Control.Monad.ST
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Data.Array.Storable
import Foreign.C.Types
import Foreign.Ptr

-- immutable arrays
buildPair :: (Int, Int)
buildPair =
  let arr = listArray (1, 10) (repeat 37) :: Array Int Int
      arr' = arr // [(1, 64)]
   in (arr ! 1, arr' ! 1)

-- mutable IO arrays
-- sikilar to IORefs
ioarray = do
  arr <- newArray (1, 10) 37 :: IO (IOArray Int Int)
  a <- readArray arr 1
  writeArray arr 1 64
  b <- readArray arr 1
  print (a, b)

-- mutable array in ST monad
-- ST is more general version of IO
buildPairST = do
  arr <- newArray (1, 10) 37 :: ST s (STArray s Int Int)
  a <- readArray arr 1
  writeArray arr 1 64
  b <- readArray arr 1
  return (a, b)

-- freezing and thawing
-- conversion between immutable and mutable arrays with freeze and thaw
-- convert array into STArray, alters it, and converts it back.
modifyAsST :: Array Int Int -> Array Int Int
modifyAsST arr = runST $
  do starr <- thaw arr
     compute starr
     newarr <- freeze starr
     return newarr

compute :: STArray s Int Int -> ST s ()
compute arr = do writeArray arr 1 64

-- storable array is an IO mutable array which stores its contents in a contiguous memory block living
-- in the C heap. Elements are stored according to the class 'Storable'. You can obtain the pointer to
-- the array contents to manipulate elements from languages like C.
-- It's slower than IOUArray but it can be passed to C routine.
-- The pointer to the array contentes is obtained by 'withStorableArray'. The idea is similar to
-- `ForeignPtr`. The pointer should be used only during execution of the IO action returned by the
-- function passed as argument to `withStorableArray`
runStorage = do
  arr <- newArray (1, 10) 37 :: IO (StorableArray Int Int)
  a <- readArray arr 1
  withStorableArray arr
    (\ptr -> memset ptr 0 40)
  b <- readArray arr 1
  print (a, b)

foreign import ccall unsafe "string.h"
  memset :: Ptr a -> CInt -> CSize -> IO ()
