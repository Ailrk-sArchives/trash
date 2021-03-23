{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ch7
  ( greeting
  , filerun
  , readLast
  , getLine'
  , putStrLn'
  , print'
  , runfs
  , withTempFile
  , fsAction
  ) where

import           Control.Exception (SomeException, catch, finally)
import           Data.Char         (toUpper)
import           System.Directory  (getTemporaryDirectory, removeFile)
import           System.IO


-- IO

greeting = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hello, " <> name


-- Working with file handlers.
-- openFile :: FilePath -> IOMode -> IO Handle
-- hClose :: Handler -> IO ()
--
-- Why hClose?
-- 1. Haskell maintains an internal buffers for files, which
--    provides a performance boost when working with file io.
--    This implies that data might not be flushed into the file
--    until hClose is called.
-- 2. An dangling Handler takes up unnecessary resources.
filerun :: IO ()
filerun = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  filerunLoop inh outh
  hClose inh
  hClose outh


filerunLoop :: Handle -> Handle -> IO ()
filerunLoop inh outh = hIsEOF inh >>= \case
    True -> return ()
    False -> do
      inpStr <- hGetLine inh
      hPutStrLn outh (toUpper <$> inpStr)
      filerunLoop inh outh


-- seek
readLast :: Int -> FilePath -> IO ()
readLast n path = do
  h <- openFile path ReadMode
  line <- lastLine n h
  print line

lastLine :: Int -> Handle -> IO String
lastLine lineWith h = do
  hSeek h SeekFromEnd (fromIntegral . negate $  lineWith)
  line <- hGetLine h
  return $ dropWhile (/= '\n') line

-- three predefined handle (file descriptors).
getLine' = hGetLine stdin
putStrLn' = hPutStrLn stdout

print' :: (Show a) => a -> IO ()
print' = hPrint stdout


-- Playing with filesystems
runfs :: IO ()
runfs = undefined

-- cheesy way of writing tmp file.
fsAction :: FilePath -> Handle -> IO ()
fsAction tmpname tmph = do
  putStrLn "Welcome to tmpfile.hs"


-- SomeException is the super class of all Other exceptions.
-- Exceptions generally used in IO monad. But there're also ways to
-- catch exceptions on pure code.
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern f = do
  tempdir <- catch getTemporaryDirectory
                   (\(_ :: SomeException) -> return ".tmp")
  (tempfile, temph) <- openTempFile tempdir pattern
  finally (f tempdir temph) (do hClose temph
                                removeFile tempfile)
