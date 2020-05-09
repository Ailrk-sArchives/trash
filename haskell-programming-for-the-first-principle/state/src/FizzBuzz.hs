module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

--------------------------------------------------------------
-- vanela fizz buzz
--------------------------------------------------------------
fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

-- mapM_ map each element to a monadic structure and
-- ignore the result.
runf :: IO ()
runf = mapM_ (putStrLn . fizzBuzz) [1..100]

--------------------------------------------------------------
-- fizzbuzz with state
--------------------------------------------------------------
-- Top lever function return a Foldable
fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []

-- append to the state each time compute a new result.
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get  -- get the state, which is the string.
  let result = fizzBuzz n
  put (result : xs)  -- add element to get new state.

--------------------------------------------------------------
-- use dlist, more efficient in appending operation.
--------------------------------------------------------------
fizzBuzzList' :: [Integer] -> DL.DList String
fizzBuzzList' list =
  execState (mapM_ addResult' list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

--------------------------------------------------------------
-- fizzbuzz exercise.
--------------------------------------------------------------
-- list range is like how you write in math.
fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to =
  execState (mapM_ addResult [to, to-1..from]) []



-- Run
runfstate :: IO ()
runfstate = mapM_ putStrLn $ reverse $ fizzBuzzList [1..10^2]

runfstate' :: IO ()
runfstate' = mapM_ putStrLn $ fizzBuzzList' [1..10^2]

runfftstate :: IO ()
runfftstate = mapM_ putStrLn $ fizzBuzzFromTo 1 100


