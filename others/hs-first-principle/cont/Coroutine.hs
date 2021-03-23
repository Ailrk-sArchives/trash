{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coroutine where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State

-- coroutine is a ContT stacked with a StateT containing the suspend coroutines.
newtype CoroutineT r m a = CoroutineT
  {runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a}
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

-- manipulate coroutine queue.
getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get

putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

dequeue :: Monad m => CoroutineT r m ()
dequeue = do
  currentCCs <- getCCs
  case currentCCs of
    [] -> return ()
    (p : ps) -> do
      putCCs ps
      p

queue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
queue p = do
  ccs <- getCCs
  putCCs (ccs ++ [p])

yield :: Monad m => CoroutineT r m ()
yield = callCC $ \k -> do
  queue (k ())
  dequeue

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork p = callCC $ \k -> do
  queue (k ())
  p
  dequeue

exhaust :: Monad m => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> getCCs
  if not exhausted
    then yield >> exhaust
    else return ()

runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

ex :: IO ()
ex = runCoroutineT $ do
  fork $ replicateM_ 3 (printOne 3)
  fork $ replicateM_ 4 (printOne 4)
  replicateM_ 2 (printOne 2)
  where
    printOne n = do
      liftIO (print n)
      yield
