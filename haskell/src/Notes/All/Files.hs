module Notes.All.Files where

import System.IO
import Control.Exception

doloop :: IO ()
doloop = do
  putStrLn "Enter a command r<filename> w<filename> or q to quit:"
  command <- getLine
  case command of
    'q':_ -> return ()

    'r':filename -> do
      putStrLn ("Reading " ++ filename)
      doRead filename
      doloop

    'w':filename -> do
      putStrLn ("Writing " ++ filename)
      doWrite filename
      doloop
    _ -> doloop

doRead :: FilePath -> IO ()
doRead filename =
  withFile filename ReadMode
          (\h -> do contents <- hGetContents h
                    putStrLn "The first 100 chars"
                    putStrLn (take 100 contents))

doWrite :: FilePath -> IO ()
doWrite filename = do
  putStrLn "Enter text to go into the file: "
  contents <- getLine
  writeFile filename (contents ++ "\n")
