module ReadingComprehension where

import Control.Applicative
import Control.Monad
import ReaderPractise

--  write your own liftA2
liftA2' :: Applicative f => (a -> b -> c)
                         -> f a -> f b -> f c
liftA2' f a1 a2 = f <$> a1 <*> a2

newtype Reader' r a =
  Reader' { runReader' :: r -> a }

instance Functor (Reader' r) where
  fmap f (Reader' ra) = Reader' $ f . ra

instance Applicative (Reader' r) where
  pure a = Reader' $ const a
  Reader' f <*> Reader' g =
    Reader' $ \r -> f r (g r)

instance Monad (Reader' r) where
  return = pure
  Reader' ra >>= arb =
    Reader' $ \ r -> runReader' (arb (ra r)) r

asks :: (r -> a) -> Reader' r a
asks = Reader'

tFuncFunctor :: IO ()
tFuncFunctor =
  print $ (runReader' $ (+1) <$> Reader' (+1)) 0

tFuncApplicative :: IO ()
tFuncApplicative =
  -- \r -> (+) r (+ 1 r)
  print $ (runReader' $ Reader' (+) <*> Reader' (+1)) 1

tFuncFunctorApplicative :: IO ()
tFuncFunctorApplicative =
  -- \r -> (+) (+ 1 r) (* 20 r)
  print $ (runReader' $ (+) <$> Reader' (+1) <*> Reader' (*10)) 2

--
shoveIn :: Reader' String String
shoveIn = Reader' (++ "++")

monadicF :: String -> Reader' String String
monadicF a = Reader' (a++)

tFuncMonad :: IO ()
tFuncMonad = do
    print $
        (runReader' $ shoveIn >>= monadicF) "<good>"
    print $
        (runReader' $ shoveIn >>= \a -> Reader' (++a)) "<good>"
