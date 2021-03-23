module Notes.Lang.Monads.StateMonad where

import Control.Monad.State

-- source
-- https://wiki.haskell.org/State_Monad

-- state game

type GameValue = Int
type GameState = (Bool, Int)  -- game is on / off & current score

playGame :: String -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score

playGame (x:xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on, score + 1)
    'b' | on -> put (on, score - 1)
    'c'      -> put (not on, score)
    _ -> put (on, score)
  playGame xs

startState = (False, 0)

run = print $ evalState (playGame "abcaaacbbcabbab") startState
