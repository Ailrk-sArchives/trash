module Main where



-- instance Functor List where
--     fmap f Nil = Nil
--     fmap f (Cons a as) =
--         Cons (f a) (fmap f as)

-- instance Applicative List where
--     pure x = Cons x Nil
--     (Cons f fs) <*> (Cons a as) =
--         Cons (f a) (fs <*> as)
--     Nil <*> _ = Nil
--     _ <*> Nil = Nil

-- instance Monad List where
--     return = pure
--     Nil >>= f = Nil
--     (Cons a as) >>= f =
--         (f a) <> (as >>= f)

-- impelment functions with monad and functor.
j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = do
    x' <- f x
    ((:) x') <$> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id

main :: IO ()
main = undefined
