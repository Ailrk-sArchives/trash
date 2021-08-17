{-# LANGUAGE RankNTypes #-}
module Types.ExScoping where

import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)

-- how ST monad works to prevent info leak out of the scope.

-- Identity monad with a phanton type s
newtype ST s a = ST { unsafeRunST :: a }

-- to use unsafePerformIO we need to make sure arguments are strict to avoid
-- leaks.
instance Functor (ST s) where
  fmap f (ST a) = a `seq` ST (f a)

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = f `seq` a `seq` ST (f a)

instance Monad (ST s) where
  ST a >>= f = a `seq` f a

-- STRef also has phantom type s. One STRef can only works on it's
-- ST monad.
newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref (f a)

-- entrance. we mark s higher rank, so each ST monad got run through runST
-- will have different s. Thus a STRef for s1 cannot be used for STRef for
-- s2.
-- s only exists within ST s a
runST :: (forall s. ST s a) -> a
runST = unsafeRunST

safe1 :: ST s String
safe1 = do
  ref <- newSTRef "good"
  modifySTRef ref (++ "hello")
  readSTRef ref

-- >>> runST safe1
-- "goodhello"

-- >>> runST (newSTRef "asd")
-- Couldn't match expected type ‘ST s a’ with actual type ‘()’


