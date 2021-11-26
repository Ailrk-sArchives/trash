module Nice.LoopBreak where


import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State

import           Data.Functor.Identity

import qualified Data.ByteString.Char8      as S8
import           Data.Function              ((&))
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)


say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

-- Note here we are not using monad transformer so everthing needs to be lifted mannually.

-- break the loop of arbitrary applicative.
-- a nice little use of polymorphism.

-- m can be any monads.
loop :: Monad m => ExceptT e m a -> m e
loop = fmap (either id id) . runExceptT . forever

quit :: Monad m => e ->  ExceptT e m a
quit = throwE

worker :: Chan Int -> Int -> IO ()
worker chan num = loop $ do
  i <- lift $ readChan chan
  when (i == (-1)) $ do     -- little sentinel
    lift $ writeChan chan (-1)
    quit ()
  lift . say . pack . mconcat $
    [ "Worker #"
    , show num
    , " received value "
    , show i ]


run1 :: IO ()
run1 = do
  chan <- newChan
  mapConcurrently (worker chan) [1..5] `concurrently`
    mapM_ (writeChan chan) ([1..20] ++ [(-1)])
  return ()

-- use ExceptT loop to calculate fib with dp
-- see there are three layers here. But we can still work with
-- it quit easily.
-- the key of dealing effects with monad transformers is to
-- understand the logic behind wrap and unwrap. It's quit linear so not
-- to hard.

fibdp :: Int -> [Integer]
fibdp n = runIdentity $ go [0, 1] [] n
  where
    go :: [Integer] -> [Integer] -> Int -> Identity [Integer]
    go s m i = flip evalStateT (s, m, i) . loop $ do
      state <- lift get
      case state of
        ([a, b], res, i) -> do
          let c = a + b
          if i == 0 then quit res else do lift $ put ([b, c], a:res, i - 1)
        _ -> quit []
