{-# LANGUAGE OverloadedStrings #-}

module Concurrent.NoclosableChannel where

-- the default putStrLn for string works on lazy list and print char by cahr,
-- Text.putStrLn by default uses nobuffering.
-- this is an attempt to implement thread safe print line function.
-- https://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics/

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad            (forever)
import qualified Data.ByteString.Char8    as S8
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (encodeUtf8)


say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

{-@ Naive implementation that blocks @-}

workerNaive :: Chan Int -> Int -> IO ()
workerNaive chan num = forever $ do
  i <- readChan chan
  say . pack . mconcat $
    [ "Worker #"
    , show num
    , " received value "
    , show i
    ]

-- producers don't know if consumers are finished, this will hang.
runNaive :: IO ()
runNaive = do
  chan <- newChan
  mapConcurrently (workerNaive chan) [1..5] `concurrently` mapM_ (writeChan chan) [1..10]
  return ()




