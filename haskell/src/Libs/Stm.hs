module Libs.Stm where

-- https://www.fpcomplete.com/haskell/library/stm/

-- these are higher level concurrent abstraction, you don't particularly need
-- to write locks.
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import qualified Data.ByteString.Char8    as S8
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T

import           System.IO.Unsafe         (unsafePerformIO)


import           Control.Monad

-- actions need to be performed atomically are group together.
-- each write to a varaible is tracked. After the block is done, the runtime will check if
-- any value is changed again. If there is any change, means a race condition,
-- thus we need to discard the current result and rerun.
-- Otherwise we can go ahead ahd commit the change into the memory.

-- STM keeps trakc of mutation on TVar.

say :: T.Text -> IO ()
say = S8.putStrLn . T.encodeUtf8

{-@ atomically works kind like performUnsafeIO or runST ...
    IO (IO Int) let us hold the reference to the same TVar in
    the IO value returned.
@-}
makeCounter :: IO (IO Int)
makeCounter = do
  var <- newTVarIO 9
  return . atomically $ do
    i <- readTVar var
    writeTVar var (i + 1)
    return i

run1 :: IO ()
run1 = do
  counter <- makeCounter
  replicateM_ 10 $ counter >>= print

{-@ failure, retry and alternative
@-}

check' :: Bool -> STM ()   -- if the state is invalid retry the transaction.
check' b = if b then return () else retry

runAliceBob :: IO ()
runAliceBob = do
  aliceVar <- newTVarIO 0     -- note this create a TVar in IO directly.
  bobVar <- newTVarIO 0
  charlieVar <- newTVarIO 0

  withAsync (pay aliceVar "alice" 100000 5) $ \_ ->
    withAsync (pay aliceVar "alice" 100000 5) $ \_ ->
      withAsync (pay bobVar "bob" 150000 8) $  \_ -> do
      atomically $ transfer 200 aliceVar charlieVar
               <|> transfer 200 bobVar charlieVar

      finalAlice <- atomically $ readTVar aliceVar
      finalBob <- atomically $ readTVar bobVar
      finalCharlie <- atomically $ readTVar charlieVar

      say . T.pack $ "Final Alice: " ++ (show finalAlice)
      say . T.pack $ "Final Bob: " ++ (show finalBob)
      say . T.pack $ "Final Charlie: " ++ (show finalCharlie)

pay :: TVar Int -> String -> Int -> Int -> IO ()
pay var name interval amount  = void . forever $ do
  threadDelay interval
  atomically $ do
    current <- readTVar var
    let updatedAmount = current + amount
    writeTVar var updatedAmount
  say . T.pack $ " paid: " ++ name

transfer :: Int -> TVar Int -> TVar Int -> STM ()
transfer amount from to = do
  currentFrom <- readTVar from
  check (currentFrom > amount)
  writeTVar from (currentFrom - amount)
  currentTo <- readTVar to
  writeTVar to (currentTo + amount)

{-@ EVIL
   why newTVarIO instead of atomically . newTVar?
   @-}

-- TVar can only be used under one atomically (their STM environment) because of their
-- implemnetation.

-- so this TVar cannot be used anywhere else since it outlive the STM environment that
-- creates it.
callCount :: TVar Int
callCount = unsafePerformIO . atomically $ newTVar 0

-- but this is ok because it's created outside of any STM.
callCount' :: TVar Int
callCount' = unsafePerformIO $ newTVarIO 0

someFunction :: TVar Int -> IO ()
someFunction c = do
  count <- atomically $ do
    modifyTVar c (+1)
    readTVar c
  putStrLn $ "this is really bad, beacuse we are using TVar from other STM"

runSomeFuncton :: IO ()
runSomeFuncton = do
  replicateM_ 10 $ someFunction callCount'  -- this works
  replicateM_ 10 $ someFunction callCount   -- this doesn't


