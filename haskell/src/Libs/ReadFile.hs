{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Libs.ReadFile where



import           Control.Exception
import           System.IO

-- Lazy IO give you suprise, in terms of (Not having RAII behavior although
-- you are using a RAII idiom)
--
-- Lazyness is a property for purity, not for side
-- effects.

doLoop = do
  putStrLn "Enter a command rFn wFN or q to quit"
  command <- getLine
  case command of
    'q':_ -> return ()
    'r':filename -> do putStrLn ("Reading " ++ filename)
                       doRead filename
                       doLoop
    'w':filename -> do putStrLn ("Writing " ++ filename)
                       doWrite filename
                       doLoop
    _ -> doLoop

doRead filename =
  bracket (openFile filename ReadMode) hClose
          (\h -> do contents <- hGetContents h
                    putStrLn "The first 100 characters: "
                    putStrLn (take 100 contents))

doWrite filename = do
  putStrLn "Enter text to go into the file:"
  contentes <- getLine
  bracket (openFile filename WriteMode) hClose
          (\h -> hPutStrLn h contentes)
