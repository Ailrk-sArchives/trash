{-# LANGUAGE ScopedTypeVariables #-}
module Libs.Refs where

{-@ There are several references type in haskell @-}

import Data.IORef
import Data.STRef
import Control.Monad.ST
import GHC.Arr
import Control.Monad


-- local state with ST.
-- this it's like the local version of unsafePerformIO.
sumST :: Num a => [a] -> a
sumST xs = runST $ do
  n <- newSTRef 0
  forM_ xs $ \x -> do
    modifySTRef n (+x)
  readSTRef n
