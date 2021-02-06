module Addition where

import Test.Hspec
import Test.QuickCheck

divBy :: Integral a => a -> a -> (a, a)
divBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

mul :: (Eq a, Num a) => a -> a -> a
mul a b
  | a == 0 = b
  | otherwise = mul (a - 1) (a * b)

-- return a Generator type Gen Int
-- return return a monad with value Int
trivialInt :: Gen Int
trivialInt = return 1

-- elements return one value from
oneToFive :: Gen Int
oneToFive = elements [1, 2, 3, 4, 5]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genChar :: Gen Char
genChar = elements ['a'..'z']

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- using quickcheck without hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater


main :: IO ()
main = hspec $ do
    describe "+" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
    describe "divBy" $ do
        it "15 divided by 3 is 5" $ do
            divBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 should be 4 reminder 2" $ do
            divBy 22 5 `shouldBe` (4 ,2)
    describe "mul" $ do
        it "2 multiply with 3 should be 6" $ do
            mul 2 3 `shouldBe` 6
    describe "property test" $ do
        it "x + 1 > x" $ do
            property $ \x -> x + 1 > (x :: Int)



