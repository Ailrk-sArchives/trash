module QuickCheck where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

-- TESTING ASSOCIATIVITY
-- if want to use quickcheck, need to be careful that a specific type is denoted
-- otherwise GHC will pick up defualt type ().
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c
-- check like this.
-- check = quickCheck (monoidAssoc :: String -> String -> String -> Bool)


-- TESTING IDENTITY
monoidIdLeft :: (Eq m, Monoid m) => m -> Bool
monoidIdLeft a = (mempty <> a) == a

monoidIdRight :: (Eq m, Monoid m) => m -> Bool
monoidIdRight a = a == (mempty <> a)

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

-- define the Arbitrary instance
instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools

instance Semigroup Bull where
    Fools <> Twoo = Twoo
    Twoo <> Fools = Twoo
    Twoo <> Twoo = Twoo
    _ <> _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- TEST FOR my VERSION OF Maybe
-- data declaration
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- define a single parameter newtype with record syntax.
-- record syntax can be very useful to create Product datatype.
-- here it is only used to access the value.
newtype First' a =
    First' { getFirst' :: Optional a }  -- value Optonal a can be accessed
                                        -- directly by getFirst' (Optional a)
    deriving (Eq, Show)

-- the helper function to generate (Optional a), by generating an arbitrary a.
genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
    x <- arbitrary  -- decude the value of arbitrary accroding to type.
    return $ Only x

-- use the previous helper function to implement Arbitrary tyclass instance
-- for (Optional a)
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary =
        frequency [ (1, genOnly)
                  , (1, return Nada)]

-- Do the same thing, implement Arbitrary for First'
-- First' as a wrapper newtype need to has arbitrary instance to do quickheck
genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
    x <- arbitrary
    return First' { getFirst' = x }
instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = genFirst


-- Semigroup is Superclass of Monoid, operations need to be defined there.
instance Monoid (First' a) where
    mempty = First' { getFirst' = Nada }

instance Semigroup (First' a) where
    -- pattern matching here also use record syntax.
    -- because you need to specify which value the pattern is corresponding with
    (First' { getFirst' = Nada }) <>
        (First' { getFirst' = Nada }) =
        (First' { getFirst' = Nada })

    (First' { getFirst' = Only a}) <>
        (First' { getFirst' = Nada }) =
        (First' { getFirst' = Only a})

    (First' { getFirst' = Nada }) <>
        (First' { getFirst' = Only a}) =
        (First' { getFirst' = Only a})

    (First' { getFirst' = Only a}) <>
        (First' { getFirst' = Only a'}) =
        (First' { getFirst' = Only a})

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
    First' String -> Bool

main :: IO ()
main = do
    -- use quick check to cheaply check the validity of laws.
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidIdLeft :: FstId)
    quickCheck (monoidIdRight :: FstId)

-- Magma: less strict than a semigroup.
