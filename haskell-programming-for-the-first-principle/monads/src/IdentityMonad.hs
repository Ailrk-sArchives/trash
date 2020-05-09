module IdentityMonad where

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = Identity
  Identity a >>= f = f a

tIdentityMonad :: IO ()
tIdentityMonad =
  putStrLn "[*] Identity Monad: " >>
    print (Identity 2 >>= \ x -> Identity $ x + 1)











