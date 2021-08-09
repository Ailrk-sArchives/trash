module EitherMonad where

data Either' e a =
    Left' e
  | Right' a
  deriving (Eq, Show)

instance Functor (Either' e) where
  fmap f (Left' e) = Left' e
  fmap f (Right' a) = Right' $ f a

instance Applicative (Either' e) where
  pure = Right'
  Right' f <*> Right' a = Right' $ f a
  Left' a <*> _ = Left' a
  _ <*> Left' a = Left' a

instance Monad (Either' e) where
  return = Right'
  Right' a >>= f = f a
  Left' a >>= _ = Left' a

type TestEither = Either' Int Int

tEitherFunctor :: IO ()
tEitherFunctor = do
  putStrLn "[*] tEitherFunctor: fmap (+1) (Right' 2 :: TestEither)"
  print $ fmap (+1) (Right' 2 :: TestEither)

  putStrLn "[*] tEitherFunctor: fmap (+1) (Left' 2 :: TestEither)"
  print $ fmap (+1) (Left' 2 :: TestEither)
  putStr "\n"

tEitherApplicative :: IO ()
tEitherApplicative = do
  putStrLn "[*] tEitherApplicative: Right' (+1) <*> (Right' 2 :: TestEither)"
  print $ Right' (+1) <*> (Right' 2 :: TestEither)

  putStrLn "[*] tEitherApplicative: Right' (+1) <*> (Left' 2 :: TestEither)"
  print $ Right' (+1) <*> (Left' 2 :: TestEither)

  putStrLn
    "[*] tEitherApplicative: Left' (+1) <*> Right' 2 :: (Either' (Int -> Int) Int)"
  print $ let res = Left' (+1) <*> Right' 2 :: (Either' (Int -> Int) Int)
              isLeft' (Left' _) = True
              isLeft' _ = False
           in if isLeft' res then "correct" else "wrong"
  putStr "\n"

tEitherMonad :: IO ()
tEitherMonad = do
  putStrLn "[*] tEitherMonad: (Right' 2 :: TestEither) >>= \\x -> Right' $ x + 1"
  print $ (Right' 2 :: TestEither) >>= \ x -> Right' $ x + 1

  putStrLn "[*] tEitherMonad: (Left' 2 :: TestEither) >>= \\x -> Right' $ x + 1"
  print $ (Left' 2 :: TestEither) >>= \ x -> Right' $ x + 1
  putStr "\n"

runEitherMonadTest :: IO ()
runEitherMonadTest = tEitherFunctor >> tEitherApplicative >> tEitherMonad

