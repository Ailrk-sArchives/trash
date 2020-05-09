module RandomExample where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- bad practice here. It is a partial function.
    x -> error $
      "intToDie got non 1 - 6 integer:" ++ show x

---------------------------------------------------
-- attemp 1 for generating random dies.
---------------------------------------------------
-- generate the same dies everytimes.
rollDiePurelyThreeTimes :: (Die, Die, Die)
rollDiePurelyThreeTimes =
  let s = mkStdGen 10
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
   in (intToDie d1, intToDie d2, intToDie d3)

---------------------------------------------------
-- attemp 2 for generating random dies.
-- Use State to control the stateful process.
---------------------------------------------------
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes =
  liftA3 (,,) rollDie rollDie rollDie

-- some test functions
-- Doesn't preserve state.
rollOnce :: Int -> (Die, Die, Die)
rollOnce r = evalState rollDieThreeTimes $ mkStdGen r

-- This only repeat a single die value rather than
-- the state action that produce a die.
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- replicateM :: Applicative m => Int -> m a -> m [a]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

-- exercises
rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n = go 0 (0, [])
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum record@(count, dies) gen
          | sum >= n = record
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die)
                   (count + 1, intToDie die : dies)
                   nextGen


