module Main where

import Control.Monad
import Control.Monad.Trans.Cont

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- make a new continuation
square :: Int -> Cont r Int
square n = return (n ^ 2)

-- simply simply return
squareCC :: Int -> Cont r Int
squareCC n = callCC $ \k -> k (n ^ 2)

-- early return
foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "here"
  return $ show $ y

-- act like goto
bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c : s
    when (s0 == "saying Hello") $ k "hello"
    let s1 = show s0
    return ("saying" ++ s1)
  return (length msg)

quux :: Cont r Int
quux = callCC $ \k -> do
  let n = 5
  k n
  return 25 -- this line will never execute.

-- complicated control flow.
fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \exit1 -> do
    when (n < 10) (exit1 (show n))
    let ns = map digitToInt (show (n `div` 2))
    n' <- callCC $ \exit2 -> do
      when ((length ns) < 3) (exit2 (length ns))
      when ((length ns) < 5) (exit2 n)
      when ((length ns) < 7) $ do
        let ns' = map intToDigit (reverse ns)
        exit1 (dropWhile (== '0') ns')
      return $ sum ns
    return $ "(ns = " ++ (show ns) ++ ") " ++ (show n)
  return $ "Answer: " ++ str
  where
    digitToInt :: Char -> Int
    digitToInt n = read $ n : []
    intToDigit :: Int -> Char
    intToDigit = head . show

-- exception
divExcept :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcept x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) $ notOk "Denominator 0"
    ok $ x `div` y
  handler err
