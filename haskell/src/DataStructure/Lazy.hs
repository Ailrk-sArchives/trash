{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE StandaloneDeriving #-}

module DataStructure.Lazy where
import Control.Monad

-------------------------------------------------------------------------------
-- Represent a suspend computation.
-- wrap wrap
data Suspend a = Suspend { unSuspend :: !(() -> a) }

instance Functor Suspend where
  fmap f (Suspend a) = Suspend $ f . a

-- effect here is just keep the value in the dummy lambda.
instance Applicative Suspend where
  pure a = Suspend $ \_ -> a
  Suspend f <*> Suspend m = Suspend (f <*> m)

instance Monad Suspend where
  return = pure
  m >>= f = f (force m)

instance Show a => Show (Suspend a) where
  show _ = "<a suspend operation>"

delay :: (() -> a) -> Suspend a
delay = Suspend

force :: Suspend a -> a
force !s = (unSuspend s) ()

-------------------------------------------------------------------------------
strictAdd :: Num a => a -> a -> a
strictAdd !a !b = a + b

-- >>> force . delay $ (\_ -> strictAdd 1 2)
-- 3

-------------------------------------------------------------------------------
-- A lazy stream
data StreamCell a = Nil | Cons !(a, Stream a) deriving Show
data Stream a = Stream !(Suspend (StreamCell a)) deriving Show

instance Semigroup (StreamCell a) where
  Nil <> c = c
  c <> Nil = c
  Cons (a, as) <> Cons (y, ys) = undefined

instance Monoid (StreamCell a) where
  mempty = Nil

instance Functor StreamCell where
  fmap f Nil = Nil
  fmap f (Cons (a, rest)) = let h = headStream rest
                             in Cons (f a, Stream . delay $ \_ -> fmap f h)

instance Applicative  StreamCell where
  pure a = Cons (a, emptyStream)
  Nil <*> Nil = Nil
  Cons _ <*> Nil = Nil
  Nil <*> Cons _ = Nil
  Cons (f, xs) <*> Cons (v, ys) = Cons (f v, consStream $ (headStream xs) <*> (headStream ys))

instance Functor Stream where
  fmap f xs = consStream (fmap f (headStream xs))

consStream :: StreamCell a -> Stream a
consStream c = Stream . delay $ \_ -> c

emptyStream :: Stream a
emptyStream = Stream . delay $ (\_ -> Nil)

fromList :: [a] -> Stream a
fromList ![]     = Stream (delay (\_ -> Nil))
fromList !(x:xs) = Stream (delay (\_ -> Cons (x, fromList xs)))

toList :: Stream a -> [a]
toList (Stream xs)
  | Nil <- force xs = []
  | Cons (a, xs) <- force xs = a : toList xs

-- >>> fromList [1, 2, 3]
-- Stream <a suspend operation>

headStream :: Stream a -> StreamCell a
headStream (Stream xs)
  | c@(Cons (_, _)) <- force xs = c
  | Nil <- force xs = Nil

tailStream :: Stream a -> Stream a
tailStream (Stream xs)
  | Cons (_, rest) <- force xs = rest
  | Nil <- force xs = emptyStream


-- >>> headStream . fromList $ [1, 2]
-- >>> headStream . fromList $ ([] :: [Int])
-- Just 1
-- Nothing

takeStream :: Int -> Stream a -> Stream a
takeStream n (Stream xs)
  | (Cons (a, rest)) <- force xs, n == 1 = consStream (Cons (a, emptyStream))
  | (Cons (a, rest)) <- force xs =  consStream (Cons (a, (takeStream (n - 1) rest)))
  | Nil <- force xs = Stream xs


-- >>> toList . takeStream 3 . fromList $ [1, 2, 3, 4, 5]
-- >>> toList . takeStream 1 . fromList $ [1, 2, 3, 4, 5]
-- [1,2,3]
-- [1]
