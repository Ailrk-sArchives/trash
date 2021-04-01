module Kingsgame where

import Data.List
import Debug.Trace

-- https://vijos.org/p/1779
-- a king playing game with ministers. He writes an integer
-- on both left hand and write hand of every ministers and himself, and
-- arrange everybody to stand in a queue. Then he will give each minister
-- bonus, the amount a minister can get is the same as the sume of all left
-- hand integers of people in front of him divided by his right hand integere.
-- the king doesn't want to make someone get too much bouns, so he wants to find
-- an order to minimize the maximum bonus.
-- What should the king do?

-- It's a simple optmization algorihtm that can be solved with greedy method pretty
-- elegantly.

-- Idea: If we try to swap two people the award gets smaller, then we do the swap.
--
-- let ai bi be the integer on the left hand and the right hand.
-- let s = sum of all left hand integers.
-- For person ai, the bounus will be:
--    s / bi
-- for person ai+1 the bonous will be:
--    (s . ai) / bi+1
--
-- If we swap ai and ai+i, the cost will be
--    s / bi+1
--  and
--    (s . ai) / bi
--
-- We can determine if we should swap these two people by the following criteria:
--    max (s / bi, (s . ai) / bi+1) < max (s / bi+1, (s . ai) / bi)
--
-- simply we can get
--    max(bi+1, ai+1 . bi) < max(bi, ai+1 . bi+1)

data M = M
  { ma :: Int,
    mb :: Int
  }
  deriving (Eq, Show)

instance Ord M where
  M a b <= M a' b'
    | a == b && a' == b' = True
    | otherwise = max b' (a * b) < max b (a' * b')

-- trick: one catch for take, it take n element instead of nth element.
-- If input is an index you need to add 1.

-- trick: use let binding to introduce intermediate values, so we can trace them
-- individually.

-- trick: let binding is actually good for readablity.
maxReward :: [M] -> Int
maxReward ms =
  let s n = sum . take (n + 1) . fmap ma $ ms
      reward n =
        let s' = s n
            b' = mb (ms !! n)
         in s' `div` b'
      rewards = fmap reward [0 .. length ms - 1]
   in maximum rewards

input = [M 1 1, M 2 3, M 7 4, M 4 6]

ordered = sort input
