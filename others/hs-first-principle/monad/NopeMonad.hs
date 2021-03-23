module NopeMonad where


-- a monad doing nothing.
data Nope a = NopeDog deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDog = NopeDog

instance Applicative Nope where
  pure _ = NopeDog
  NopeDog <*> NopeDog = NopeDog

instance Monad Nope where
  return _ = NopeDog
  NopeDog >>= _ = NopeDog


tNopeMonadFunctor :: IO ()
tNopeMonadFunctor = do
  putStrLn "[*] tNopeMonadFunctor: "
  print $ fmap (+1) NopeDog
  putStr "\n"

tNopeMonadApplicative :: IO ()
tNopeMonadApplicative = do
  putStrLn "[*] tNopeMonadApplicative: "
  print $ NopeDog <*> NopeDog
  putStr "\n"

tNopeMonad :: IO ()
tNopeMonad = do
  putStrLn "[*] tNopeMonad: "
  print $ NopeDog >>= const NopeDog
  putStr "\n"

runNopeMonadTest :: IO ()
runNopeMonadTest = do
  tNopeMonadFunctor
  tNopeMonadApplicative
  tNopeMonad

