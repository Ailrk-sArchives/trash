module Functor where

import Test.QuickCheck
-- Functor penerates the structure and apply function to
-- the content within the structure.
-- f a, kind of f must be * -> *

data FM a =
    FM
  | Pls a
  deriving (Eq, Show)


-- note it is FM not (FM a)
-- otherwise a will become a part of the functor sturcture,
-- the kind of (FM a) is *, but to be a functor it need to have
-- kind * -> *.
-- (FM a) will not compile.
instance Functor FM where
    fmap _ FM = FM
    fmap f (Pls a) = Pls (f a)

-- lift twice type checking.
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--         fmap         fmap
-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y
-- g (f y)

data Two a b = Two a b deriving (Eq, Show)
data Or a b = Left' a
            | Right' b
            deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = (Two a) $ (f b)

instance Functor (Or a) where
    fmap _ (Left' a) = Left' a
    fmap f (Right' b) = Right' (f b)

-- quickcheck
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Eq (f c), Functor f) =>
                        (a -> b) -> (b -> c) -> f a -> Bool
functorComposition f g x =
    fmap g (fmap f x) == fmap (g . f) x

main :: IO ()
main = do
    quickCheck $ \x -> functorIdentity (x :: String)
    quickCheck $ \x -> functorComposition (+1) (*2) (x :: Maybe Int)

