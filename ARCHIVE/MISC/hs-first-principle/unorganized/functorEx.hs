module FunctorEx where

import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

-- Identity
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

genId :: Arbitrary a => Gen (Identity a)  -- Generate one arbitrary Identity a
genId = do
    a <- arbitrary
    return $ Identity a
instance Arbitrary a => Arbitrary (Identity a) where  -- typeclass for Arbitrary
    arbitrary = genId

-- Pair
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)
genPair :: Arbitrary a => Gen (Pair a)
genPair = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = genPair

-- Two
instance Functor (Two a) where
    fmap f (Two a b) = (Two a) $ (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

instance Functor (Three a b) where
    fmap f (Three a b c) = (Three a b) $ (f c)
genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
        Arbitrary (Three a b c) where
            arbitrary = genThree

 -- Three'
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)
genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = genThree'

-- Four
instance Functor (Four a b c) where
    fmap f (Four a b c d) = (Four a b c) $ (f d)
genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
           Gen (Four a b c d)
genFour = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = genFour

-- Four'
instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
genFour' :: (Arbitrary a, Arbitrary b) =>
            Gen (Four' a b)
genFour' = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = genFour'

-- functor check
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Functor f, Eq (f c)) =>
                         f a -> Fun a b -> Fun b c -> Bool
functorComposition x (Fun _ f) (Fun _ g) =
    (fmap g . fmap f) x == (fmap (g . f) x)

-- checking type signatures
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFC' = Three' Int Int -> IntToInt -> IntToInt -> Bool
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool
type FourFC' = Four' Int Int -> IntToInt -> IntToInt -> Bool


main :: IO ()
main = do
    putStrLn "Functor [Int]"
    quickCheck (functorIdentity :: ([Int] -> Bool))
    quickCheck (functorComposition :: IntFC)
    putStrLn "Identity [Int]"
    quickCheck (functorIdentity :: (Identity Int -> Bool))
    quickCheck (functorComposition :: IdFC)
    putStrLn "Pair [Int]"
    quickCheck (functorIdentity :: (Pair Int -> Bool))
    quickCheck (functorComposition :: IdFC)
    putStrLn "Two [Int]"
    quickCheck (functorIdentity :: (Two Int Int -> Bool))
    quickCheck (functorComposition :: TwoFC)
    putStrLn "Three [Int]"
    quickCheck (functorIdentity :: (Three Int Int Int -> Bool))
    quickCheck (functorComposition :: ThreeFC)
    putStrLn "Three' [Int]"
    quickCheck (functorIdentity :: (Three' Int Int -> Bool))
    quickCheck (functorComposition :: ThreeFC')
    putStrLn "Four [Int]"
    quickCheck (functorIdentity :: (Four Int Int Int Int -> Bool))
    quickCheck (functorComposition :: FourFC)
    putStrLn "Four' [Int]"
    quickCheck (functorIdentity :: (Four' Int Int -> Bool))
    quickCheck (functorComposition :: FourFC')



