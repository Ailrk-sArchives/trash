module PL.CoroutinesWithContinuation where


import           Control.Applicative
import           Control.Monad.Cont
import           Control.Monad.State

-- ContT stacked with stateT contains suspended coroutines.
newtype CoroutineT r m a = CoroutineT
  { runCoroutineT :: ContT r (StateT [CoroutineT r m ()] m) a }

getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get

putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put
