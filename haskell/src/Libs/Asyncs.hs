{-# LANGUAGE RankNTypes #-}
module Libs.Asyncs where

import qualified Conduit                  as C
import           Control.Applicative
import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as Async
import           Control.Exception
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
  where
    action1 :: IO Int
    action1 = do
      CC.threadDelay 80000
      return 5
    action2 :: IO String
    action2 = do
      CC.threadDelay 1000000
      return "action 2"


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

-- note we need to wrap IO () in Concurrently newtype to work on a collection.
-- concurrently function is just a binary operation.
-- To work on multiple values we need an applicative
writePeople :: [Person] -> IO ()
writePeople = Async.runConcurrently . traverse_ (Async.Concurrently . writePerson)

writeMany :: IO ()
writeMany = writePeople ppls

{-@ handle exception
    when a child thread throws an exception, it's thrown to the other thread.
@-}

handleException :: IO ()
handleException = do
  res <- Async.concurrently action1 action2
  print res
  where
    action1 :: IO Int
    action1 = throwIO (Async.AsyncCancelled)  -- throw exception

    action2 :: IO String
    action2 = handle onerr $ do
      CC.threadDelay 500000
      return "acton2 complete"
      where
        onerr e = do
          putStrLn $ "actoin2 was killed by: " ++ displayException e
          throwIO (e :: SomeException)

{-@ Companion infinite threads
    (detach async version)
@-}

counter :: IO a   -- this runs forever.
counter =
  let loop i = do
        putStrLn $ "counter: " ++ show i
        CC.threadDelay 1000000
        loop $! i + 1
   in loop 1

-- inner never return eailer than counter
-- nice!
withCounter :: IO a -> IO a
withCounter inner = do
  res <- Async.race counter inner
  case res of
    Left x  -> assert False x
    Right x -> return x

runCompanion :: IO ()
runCompanion = do
  putStrLn "Before withCounter"
  CC.threadDelay 2000000
  withCounter $ do
    withDelay 2000000 (putStrLn "Inside withCounter")
  withDelay 2000000 (putStrLn "After withCounter")
  putStrLn "Bye!"
  where
    withDelay :: Int -> IO () -> IO ()
    withDelay t a = CC.threadDelay t >> a >> CC.threadDelay t

{-@
@-}

-- TODO
