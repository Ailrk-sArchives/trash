{-# LANGUAGE ExplicitForAll #-}
module Libs.MonadBaseControl where

import Control.Monad.Trans.Control

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.IO
import Control.Monad.State

-- simple intuition
data E = Error

-- How do we have these two functions work together?
sayHiError :: Handle -> ExceptT E IO ()
sayHiError handle = lift $ hPutStrLn handle "Hello"

withFile' :: (Handle -> IO a) -> IO a
withFile' = withFile "text.txt" WriteMode

useFileError1 ::  ExceptT E IO ()
useFileError1 = rewrapped
  where
    unwrapped :: Handle -> IO (Either E ())
    unwrapped handle = runExceptT $ sayHiError handle
    applied :: IO (Either E ())
    applied = withFile' unwrapped
    rewrapped = ExceptT applied

-- wrap sayHiError handle in a lambda and unwrap inside, then wrap the result.
useFileError1Inlined :: ExceptT E IO ()
useFileError1Inlined = ExceptT (withFile' (\handle -> runExceptT $ sayHiError handle))


-- use monad trans control to wrap and unwrap stuffs.

-- with MonadTrans we can lift (IO r) into (t IO r)
withFileLifted :: MonadTrans t => FilePath -> IOMode -> (Handle -> IO r) -> t IO r
withFileLifted file mode action = lift $ withFile file mode action

-- with MonadTransControl we can lift (a -> t IO r) to t IO r
-- the mechanism is like stated above.
withFileLifted' :: (Monad (t IO), MonadTransControl t)
                => FilePath -> IOMode -> (Handle -> t IO r) -> t IO r
withFileLifted' file mode action = do
  lifted <- liftWith (\run -> withFile file mode (run . action))
  restoreT . return $ lifted



-- another example
-- bar has an IO base monad, so technically we can apply foo to that layer.
-- But how do we do that?
foo :: IO a -> IO a
foo = undefined

bar :: StateT Int IO a
bar = undefined

-- manually unwrap, fiddle with effect, then wrap the result.
-- in another word, we want to unlift the monad m so we can apply foo.
foo' :: StateT Int IO a -> StateT Int IO a
foo' m = do
  s <- get
  (v, s') <- lift $ foo (runStateT m s)
  put s'
  return v
