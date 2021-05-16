{-# LANGUAGE RankNTypes #-}
module Libs.Asyncs where

import qualified Conduit                  as C
import           Control.Applicative
import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as Async
import           Data.Foldable


{-@ Async prelimiary @-}

-- run action1 and two asynchronously.
asyncTemplate :: (forall a b. Show a => Show b => IO a -> IO b -> IO ()) -> IO ()
asyncTemplate action = do
  action action1 action2
  where
    action1 :: IO Int
    action1 = do
      CC.threadDelay 80000
      return 5
    action2 :: IO String
    action2 = do
      CC.threadDelay 1000000
      return "action 2"

-- concurrently runs two IO actions concurrently.
-- concurrently ::  IO a -> IO b -> IO (a, b)
basicAsync = asyncTemplate
           $ \a1 a2 -> do
             res <- Async.concurrently a1 a2
             print res

-- return the one that finished first
-- race ::  IO a -> IO b -> IO (Either a b)
-- this will alwasy return Left 5
raceAsync = asyncTemplate
          $ \a1 a2 -> do
            res <- Async.race a1 a2
            print res


{-@ Concurrently newtype wrapper.
@-}

action1 :: IO Int
action1 = do
  CC.threadDelay 80000
  return 5
action2 :: IO String
action2 = do
  CC.threadDelay 1000000
  return "action 2"

concurrentlyNewType :: IO ()
concurrentlyNewType = do
  res1 <- Async.runConcurrently $ (,)     -- applicative for concurrently
      <$> Async.Concurrently action1
      <*> Async.Concurrently action2
  print res1

  res2 <- Async.runConcurrently           -- alternative for race.
        $ (Left <$> Async.Concurrently action1)
      <|> (Right <$> Async.Concurrently action2)
  print res2


{-@ Concurrently write multiple into files
@-}
type Score = Int
data Person = P FilePath Score

ppls :: [Person]
ppls = [ P "alice.txt" 10
       , P "bob.txt" 20
       , P "cici" 30
       ]

writePerson :: Person -> IO ()
writePerson (P fp score) = writeFile fp (show score)

writePeople :: [Person] -> IO ()
writePeople = Async.runConcurrently . traverse_ (Async.Concurrently . writePerson)

writeMany :: IO ()
writeMany = writePeople ppls
