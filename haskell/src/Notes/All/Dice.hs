module Notes.All.Dice where

import Control.Monad
import Control.Applicative
import System.Random
import Control.Monad.State

-- use random with IO.
-- state is stored outside the program.
rollDice :: IO (Int, Int)
rollDice = liftA2 (,) (randomRIO (1, 6)) (randomRIO (1, 6))

-- get rid of IO
-- manage state inside the program.
-- The functor return a value and a new state.
-- Purpose of State monad is to manage the state.
-- like how to feed the new state to random again to generate new
-- random number.
clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
  where
    (n, g) = randomR (1, 6) (mkStdGen 0)
    (m, _) = randomR (1, 6) g  -- how to use the newly generated g'?
