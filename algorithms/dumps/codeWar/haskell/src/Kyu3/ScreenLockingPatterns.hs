module Kyu3.ScreenLockingPatterns where

import Data.Ix

data Vertex = A | B | C | D | E | F | G | H | I
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)


countPatternFrom :: Vertex -> Int -> Int
countPatternFrom = undefined
