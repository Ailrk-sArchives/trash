module State where

import Control.Monad.State

state1 :: Maybe ((), Int)
state1 = runStateT (put 8) 7
-- ((), 8)

state2 :: Maybe (Int, Int)
state2 = runStateT get 8
-- (8, 8)

state3 :: Maybe (Int, Int)
state3 = runStateT (put 1 >> get) 8
-- (1, 1)

state4 :: Maybe (Int, Int)
state4 = runStateT (put 1 >> put 2 >> get) 111
-- (2, 2)

state5 :: Maybe (Int, Int)
state5 = runStateT (put 1 >> get >> return 10) 112
-- (10, 112)

