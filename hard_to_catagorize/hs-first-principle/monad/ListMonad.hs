module ListMonad where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil

instance Semigroup (List a) where
  Nil <> Nil = Nil
  as <> Nil = as
  Nil <> as = as
  Cons a as <> bs =
    Cons a (as <> bs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> Cons a as =
    Cons (f a) (fmap f as <> (fs <*> as))

instance Monad List where
  return a = Cons a Nil
  Nil >>= _ = Nil
  Cons a as >>= f = f a <> (as >>= f)


testList1 = Cons 1 (Cons 2 (Cons 3 Nil))
testList2 = Cons 9 (Cons 8 (Cons 7 Nil))
testFunctionList = Cons (+1) (Cons (+2) (Cons (+3) Nil))

tListMonadMonoid :: IO ()
tListMonadMonoid = do
  putStrLn "[*] tListMonadMonoid: "
  print $ testList1 <> testList2
  print $ testList2 <> testList1
  print $ testList2 <> Nil
  putStrLn "\n"

tListMonadFunctor :: IO ()
tListMonadFunctor = do
  putStrLn "[*] tListMonadFunctor: "
  print $ (+1) <$> testList1
  putStrLn "\n"

tListMonadApplicative :: IO ()
tListMonadApplicative = do
  putStrLn "[*] tListMonadApplicative: "
  print $ testFunctionList <*> pure 1
  print $ testFunctionList <*> testList1
  putStrLn "\n"

tListMonadMonad :: IO ()
tListMonadMonad = do
  putStrLn "[*] tListMonadMonad"
  print $ testList1 >>= \ x -> Cons (x + 99) Nil
  print $ return 1 >>= \ x -> Cons (x + 99) Nil
  putStrLn "\n"

runListMonad :: IO ()
runListMonad = tListMonadMonoid >> tListMonadFunctor >> tListMonadApplicative
             >> tListMonadMonad
