module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM, join)
import System.Random
import Control.Monad.State

-- State and it's monadic interace --
-- notice State is not in-place mutation, you need ST
-- to do that.
-- State can be thought as data that exists in addition to
-- the input and output of the function that can potentially
-- change after each function is evaluated.

-- State in haskell:
--  1. let us have state doesn't require IO
--  2. is limited only to the data in our State container.
--  3. maintains referential transparency.
--  4. explicit in types

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

class ToDie a where
  toDie :: a -> Die

instance ToDie Int where
  toDie n =
    case n of
      1 -> DieOne
      2 -> DieTwo
      3 -> DieThree
      4 -> DieFour
      5 -> DieFive
      6 -> DieSix
      x -> error $ "This is a terrible error handling" ++ show x

-- state function turn a state function into a state monad transformer
rollDie :: State StdGen Die
rollDie = toDie <$> state (randomR (1, 6 :: Int))

tripleDices :: State StdGen (Die, Die, Die)
tripleDices = liftA3 (,,) rollDie rollDie rollDie

-- note to repeat the action instead of the value with `repeat`.
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollTil20 :: StdGen -> Int
rollTil20 g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

-- Exercise
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count dices gen
      | sum >= n = (count, dices)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) ((toDie die) : dices) nextGen

-- write your own State --
newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap f (Moi g) =
    Moi $ (\(a, s) -> (f a, s)) <$> g

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (ab, s') = f s
          (a, s'') = g s
       in (ab a, s'')

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g =
    Moi $ \s ->
      let (a, s') = f s
       in  (runMoi $ g a) s'

-- fizzbuzz with state --
fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Fizz"
  | n `mod` 3 == 0 = "Buzz"
  | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
fizzBuzz100 = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]

-- Exercise (implement some state accessors) --
mget :: Moi s s
mget = Moi $ \s -> (s, s)

mput :: s -> Moi s ()
mput s = Moi $ \s' -> ((), s)

mexec :: Moi s a -> s -> s
mexec (Moi sa) s = snd $ sa s

meval :: Moi s a -> s -> a
meval (Moi sa) s = fst $ sa s

mmodify :: (s -> s) -> Moi s ()
mmodify f = Moi $ \s -> ((), f s)

main :: IO ()
main = putStrLn "Hello, Haskell!"
