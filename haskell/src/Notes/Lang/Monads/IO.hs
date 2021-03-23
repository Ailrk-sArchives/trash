module Notes.Lang.Monads.IO where

import Data.Char (toUpper)
import Control.Monad

-- IO is not a function, it is an action.
-- All haskell functions are pure
-- Effecful actions are sealed in type.
-- and the pure function handle the effecful value by
-- manipulating the monad.

toUpperCase :: IO ()
toUpperCase = putStrLn "Write your string: " >> fmap shout getLine >>= putStrLn
  where
    shout = map toUpper

fiveGetLines = replicateM 5 getLine
