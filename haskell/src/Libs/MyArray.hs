{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Libs.MyArray where

{-@ My array implemnetation
    the array package is implemented based on GHC.IOArray and GHC.Arr,
    which in turn are implemented based on Arrays implemented on GHC.Exts.
    Because a mutable array is fundamentally mutable, you need some ghc support
    to work with it.
 @-}


import           Control.Monad.ST.Lazy
import qualified Control.Monad.ST.Lazy as Lazy (ST)
import           Data.Ix               (Ix, index, range, rangeSize)
import           Foreign.C.Types
import           Foreign.Storable
import           GHC.Base              (IO (..), divInt#)
import           GHC.ST                (ST (..), runST)
