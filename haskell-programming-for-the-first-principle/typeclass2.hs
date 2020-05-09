module Typeclass2 where

class Numberish a where
    fromNum :: Integer -> a
    toNum :: a -> Integer
    defaultNum :: a

newtype Age = Age Integer deriving (Eq, Show)
newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Age where
    fromNum = Age
    toNum (Age n) = n
    defaultNum = Age 65  -- will not provide enough info for type inferrence

instance Numberish Year where
    fromNum = Year
    toNum (Year n) = n
    defaultNum = Year 1998

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNum summed
    where integerOfA = toNum a
          integerOfAPrime = toNum a'
          summed = (+) integerOfA integerOfAPrime




