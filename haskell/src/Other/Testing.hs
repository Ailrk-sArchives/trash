{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Other.Testing where
import           Data.Char
import           Test.QuickCheck

-- how to test these function?
-- especially we have a getChar which performs
-- side effect
--
-- Here comes another topic, how to write testable code.
-- It's node clear how to properly test this one becuase
-- side effect and pure code a tangled together.
getList' = find 5 where
  find 0 = return []
  find n = do
    ch <- getChar
    if ch `elem` ['a'..'e']
       then do
         tl <- find (n - 1)
         return (ch : tl)
       else find n

newtype Ch = Ch Char deriving newtype (Eq, Show)
-- by separating the big IO function into two smaller one, at least
-- we can test the take 5 one easily.
getList :: IO [Ch]
getList = fmap take5 $ ((fmap . fmap) Ch getContents)

take5 :: [Ch] -> [Ch]
take5 = take 5 . filter (`elem` (fmap Ch ['a'..'e']))

{-@ Testing with quickcheck
    First implement Arbitrary typeclass for data you wnat
    to simulate
@-}

instance Arbitrary Ch where
  arbitrary = Ch <$> choose ('\32', '\128')

instance CoArbitrary Ch where
  coarbitrary (Ch c) = variant (ord c `rem` 4)

-- now try to test some properties
identity = quickCheck ((\s -> s == s) :: [Ch] -> Bool)
doubleReverse = quickCheck ((\s -> (reverse . reverse) s == s) :: [Char] -> Bool)
testTake5 = quickCheck $ (<=5) . length . take5
testRightChar = quickCheck $ \s -> all (`elem` (fmap Ch ['a' .. 'e'])) (take5 s)

testTake5Verbose = verboseCheck $ (<5) . length . take5
