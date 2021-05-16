{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Libs.Asyncs where

import qualified Conduit                     as C
import           Control.Applicative
import qualified Control.Concurrent          as CC
import qualified Control.Concurrent.Async    as Async
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Foldable
import           Data.Void


import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           System.IO

{-@ simple @-}

lazyStringIsNotThreadSafe = Async.mapConcurrently_  worker [1..10]
  where
    worker n = replicateM_ 5 $ putStrLn $ "Hi I'm worker " ++ show n

textByDefaultAlsoNotThreadSafe = Async.mapConcurrently_ worker [1..10]
  where
    worker n = replicateM_ 5
             $ T.putStrLn
             $ T.pack
             $ "Hi I'm worker " ++ show n

setBufferingSolvesIt = do
  hSetBuffering stdout LineBuffering
  Async.mapConcurrently_ worker [1..10]
  hSetBuffering stdout NoBuffering
  where
    worker n = replicateM_ 5
             $ T.putStr
             $ T.pack
             $ "Hi I'm worker " ++ show n ++ "\n"

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
  return $ either (const $ absurd (error "never happen")) id res

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

{-@ Use monad control to run companion thread with another read that's
    on some monad transformer stack.
@-}

withCounter' :: MonadBaseControl IO m => m a -> m a
withCounter' inner = control $ \runInIO -> do   -- cps based
  res <- Async.race counter (runInIO inner)
  return $ either (const (absurd (error "never happen"))) id res

runCompanionAnotherThreadNotInIO :: IO ()
runCompanionAnotherThreadNotInIO = do
  putStrLn "Before withCounter'"
  CC.threadDelay 2000000
  flip runReaderT "some string" $ withCounter' $ do
    liftIO $ CC.threadDelay 2000000
    str <- ask
    liftIO $ putStrLn $ "Inside with string: " ++ str
    liftIO $ CC.threadDelay 2000000
  CC.threadDelay 2000000
  putStrLn "After withCounter'"
  CC.threadDelay 2000000
  putStrLn "Exit"

runCompanionNoCounter :: IO ()
runCompanionNoCounter = do
  putStrLn "Before withCounter'"
  CC.threadDelay 2000000
  flip runReaderT "some string" $ do
    liftIO $ CC.threadDelay 2000000
    str <- ask
    liftIO $ putStrLn $ "Inside with string: " ++ str
    liftIO $ CC.threadDelay 2000000
  CC.threadDelay 2000000
  putStrLn "After withCounter'"
  CC.threadDelay 2000000
  putStrLn "Exit"


{-@ Async represents an action running in a different thread which if success will
    give an result of type a.
@-}

