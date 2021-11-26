{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Libs.FusedEffect where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad.IO.Class       (MonadIO (liftIO))

--------------------------------
-- basics

-- create effect actions
action1 :: Has (State String) sig m => m ()
action1 = do
  s <- get
  put ("hello, " ++ s)


action2 :: (Has (State String) sig m, Has (Reader Int) sig m) => m ()
action2 = do
  i <- ask
  put (replicate i '!')


-- runState has type  :: s -> StateC s m a -> m (s, a).
-- it takes the initial state and a state carrier, which is an action
-- that has state effect.


-- run with effect handler
example1 :: (Algebra sig m) => [a] -> m (Int, ())
example1 xs = runState 0 $ do
  i <- get
  put (i + length xs)


-- you can stack effects with handlers.
example2 :: (Algebra sig m) => m (Int, ())
example2 = runReader "hello" . runState 0 $ do
  xs :: String <- ask
  put (length xs)


example3 :: (Int, ())
example3 = run . runReader "hello" . runState 0 $ do
  xs :: String <- ask
  put (length xs)


example4 :: IO (Int, ())
example4 = runM . runReader "hello" . runState 0 $ do
  xs <- ask
  liftIO (putStrLn xs)
  put (length xs)


--------------------------------
--
