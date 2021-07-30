{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniProject.CoroutinesCPS where

import           Control.Applicative
import           Control.Concurrent  (threadDelay)
import           Control.Monad.Cont
import           Control.Monad.State
import           Debug.Trace

-------------------------------------------------------------------------------
-- coroutine with ContT
-- -- what is coroutine?
-- cooperative multitasking, each coroutine coperatively give away control.
-- This implies we need some way to volunturily give away control from one
-- thread of execution to the other.
--
-- In imperative programming this might be implemented by goto or long jump.
-- But we are in haskell, so we use cps for arbitray transfer of control flow.
--
-- -- Fun fact about coroutines
-- Coroutines, as oppose to subroutine, yeild controls to each other instead of
-- return control from callee to the caller.
-- The concept of coroutine has been around since 1958, one year after the
-- invention of fortran!
--
-- Coroutines forms nodes
--
-- -- Application of coroutines
--



-------------------------------------------------------------------------------
-- ConT to transfer control among coroutines.
-- All coroutines are stored in a lists and be accessed from the state monad.
-- The scheduling is just simple FCFS.
newtype CoroutineT r m a =
  CoroutineT { runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a }
  deriving ( Functor, Applicative, Monad
           , MonadCont
           , MonadIO
           , MonadState [CoroutineT r m ()])

-- operaionts on the queue
dequeue :: Monad m => CoroutineT r m ()
dequeue = do
  currentCCs <- get
  case currentCCs of
    [] -> return ()
    (x:xs) -> do
      put xs
      x

queue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
queue p = do
  ccs <- get
  put (ccs ++ [p])  -- enqueue to the end

-- yield out the control
yield :: Monad m => CoroutineT r m ()
yield = callCC $ \k -> do
  queue (k ())  -- enqueue cc. this yield the control
  dequeue       -- dequeue the next coroutine in the queue

-- push a new task into the queue.
fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork p = callCC $ \k -> trace (show "forking") $ do
  queue (k ())    -- enqueue cc. again, yield the control
  p               -- once yield back, run p (p might yield itself)
  dequeue         -- dequeue the next coroutine

-- pass the control to suspended coroutines repeatedly until there's no more
-- coroutine left
exhaust :: Monad m => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> get
  if not exhausted
     then yield >> exhaust    -- keep yielding until the queue is empty.
     else return ()

-- entry
runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

-- test
printOne :: (MonadIO m, Show a) => a -> CoroutineT r m ()
printOne n = trace ("printOne " ++ show n) $ do
  liftIO (print n)
  yield
  liftIO (threadDelay (1000000 `div` 4))

-- tasks yields in order.
example1 :: IO ()
example1 = runCoroutineT $ do
  fork $ replicateM_ 3 (printOne 5)
  fork $ replicateM_ 3 (printOne 10)
  replicateM_ 2 (printOne 2)

-------------------------------------------------------------------------------
-- The cnotrol flow:
-- fork $ replicateM_ 3 (printOne 5):
--  run (printOne 5) three times.
--  each fork  will enqueue
--
--
