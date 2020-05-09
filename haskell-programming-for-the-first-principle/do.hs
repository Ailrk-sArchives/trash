module Do where

import System.IO
import Control.Monad (when)

getName :: IO ()
getName = do
    hSetBuffering stdout NoBuffering
    putStr "Your Name: "
    name <- getLine
    putStrLn name

getName' :: IO ()
getName' =
  hSetBuffering stdout NoBuffering >>
    putStr "Your Name " >>
      getLine >>=
        \name ->
          putStrLn name

addStr :: IO String
addStr = do
    x1 <- getLine
    x2 <- getLine
    putStrLn (x1 ++ x2)
    return (x1 ++ x2)

addStr' :: IO String
addStr' =
  getLine >>=
    \x1 ->
      getLine >>=
        \x2 ->
          putStrLn (x1 ++ x2) >> return (x1 ++ x2)

twoo :: IO ()
twoo = do c <- getChar
          c' <- getChar
          when (c == c') $ putStrLn "True"
          -- if (c == c')
          --    then putStrLn "True"
          -- else return ()

twoo' :: IO ()
twoo' =
  getChar >>=
    \c ->
      getChar >>=
        \c' ->
          when (c == c') $ putStrLn "True"





