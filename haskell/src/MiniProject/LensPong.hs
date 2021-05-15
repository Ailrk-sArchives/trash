{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
module MiniProject.LensPong where


import Control.Lens hiding ((:>), at)
import Control.Monad.State (State, execState, get)
import Control.Monad (when)

import Data.Set (Set, empty)
import Data.Conduit
