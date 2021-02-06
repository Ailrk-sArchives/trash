module Concurrent.Concurrent where


import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import           Data.IORef
import           System.IO

{-@ MVar t
    A mutable that is either empty or contains a value of t. MVar is the
    most primitive notion in haskell concurrency model.

    takeMVar will empty MVar if there is value in it, otherwise it will block.
    putMVar will fill the MVar if it's empty, otherwise block.

    MVar's usage
      1. sychronized mutable variable
      2. as a channel.
      3. MVar () as a binary semaphore

    Note:
      Value in MVar is lazy, but there is also strict MVar.
      You can rely on the lexical order of MVar.
@-}

-- use MVar as mutex
mutex = newEmptyMVar :: IO (MVar ())
acquire m = putMVar m ()
release m = takeMVar m

-- Skip channel
-- transmit source of high bandwidth information (like mouse movement)
-- Contains two MVars
-- first MVars has it's current value, and a list of semaphores to notify
-- second MVars is a semaphore for this particular reader. Full if there is a value
-- not read yet.
-- Writing to the channel never block. Reading block if there are no new values.

data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())

-- create an empty skip channel.
newSkipChan :: IO (SkipChan a)
newSkipChan = do
  sem <- newEmptyMVar
  main <- newMVar (undefined, [sem])
  return $ SkipChan main sem

putSkipChan :: SkipChan a -> a -> IO ()
putSkipChan (SkipChan main _) v = do
  (_, sems) <- takeMVar main
  putMVar main (v, [])
  mapM_ (\sem -> putMVar sem ()) sems   -- release all.

-- note if get from a newly created skip chan,
-- it will hit the bottom.
getSkipChan :: SkipChan a -> IO a
getSkipChan (SkipChan main sem) = do
  takeMVar sem
  (v, sems) <- takeMVar main
  putMVar main (v, sem:sems)
  return v

-- multiple readers
dupSkipChan :: SkipChan a -> IO (SkipChan a)
dupSkipChan (SkipChan main _) = do
  sem <- newEmptyMVar
  (v, sems) <- takeMVar main
  putMVar main (v, sem:sems)
  return $ SkipChan main sem


{-@ Threads
    Haskell has both lightweight thread and OS thread.
    Lightweight thread system schedule logical threads on available
    Operating system threads.

    Lightweight threads are also called unbound thread, while
    a native operating system thread is called an bound thread, since
    it bounds to one os thread.

    Unbound threads are manged in a cooperative fashion by the rts's IO manager.
    When one unbound thread is blocked or locked it will call `yield` and yield
    to another runnable thread.

    A thread can wait with `threadDelay`

@-}


{-@ IORef
    Simplest mutable reference.
    It's like unprotected shared memory in most other languages.
    You might want a `MVar ()` to provide mutual exclusion on a
    concurrent read/write on it.

    you also have `atomicWriteIORef` and `atomicModifyIORef` to
    perform atomic operatoins.
@-}

iorefDemo :: IO Integer   -- it's
iorefDemo = do
  account1 <- newIORef  1000
  account2 <- newIORef  2000
  transfer 400 account1 account2
  readIORef account1
  where -- thread safe transfer
    transfer :: Integer -> IORef Integer -> IORef Integer -> IO ()
    transfer n from to = do
      atomicModifyIORef from $ (flip (,) ()) . (+ (- n))
      atomicModifyIORef to $ (flip (,) ()) .(+ n)


{-@ TVar and STM
    A transactional mutable variable. A TVar is able to be read and written
    in the STM monad.

    The idea of STM is to treat memory as database and operation to access
    then as transactions.
    STM provides two key operations:
      1. atomically
      2. retry:      rerun when the runtime fails to commit a transaction.

    Besides being able to be used in STM, TVar also has the same semantics
    as MVar. You can use `putTMVar`, `takeTMVar`, and use it exactly like
    a MVar.

  STM:
    focus on atomicity, consistency, and isolation.
    There can be multiple potential "transactions" waiting to get
    executed concurrently.
    If two transactions doesn't use any shared TVars at the same time,
    they will commit their transaction and finish execution.
    If two transactions are both using a shared TVar and conflict occurs,
    this transaction fails and get discard, and need to be retried.

  Difference between TVar and MVar.
    1. TVar live in STM, which make sure their will be no deadlock.
       MVar is just a value assoicated with a lock, and can cause deadlock.
    2. TVar ais less performed than MVar
    3. In STM, a long runing transaction is possible to starve other short
       running transactions (it's a scheduling problem after all).
@-}

tvarDemo :: IO Integer
tvarDemo = do
  account1 <- atomically $ newTVar 1000
  account2 <- atomically $ newTVar 2000
  atomically $ transfer 500 account1 account2 >> readTVar account1
  where
    transfer :: Integer -> TVar Integer -> TVar Integer -> STM ()
    transfer n from to = do
      modifyTVar from (+ (-n))
      modifyTVar to (+ n)

{-@ Chan
    Unbounded queues implemented with MVars
@-}
chanDemo :: IO ()
chanDemo = do
  hSetBuffering stdout LineBuffering
  run
  where
    run = do
      chan <- newChan
      sequence [forkIO (consumer chan) | _ <- [0..3]]
      forkIO $ producer chan
      pure ()
    producer :: Chan Integer -> IO ()
    producer chan = forM_ [1..10000] $ \i -> do
      writeChan chan i
      putStrLn "write to channel"
    consumer :: Chan Integer -> IO ()
    consumer chan = forever $ do
      val <- readChan chan
      thread <- myThreadId
      putStrLn $ "Received: " ++ show val ++ ", tid: " ++ show thread
