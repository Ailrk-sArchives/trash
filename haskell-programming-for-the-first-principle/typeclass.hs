module Typeclass where

data Trivial = Trivial'
instance Eq Trivial where
    Trivial' == Trivial' = True

-- sum type. the equivalence Enum in C is actually a partial function.
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

data Date =
    Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
             weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'


-- exercises
-- write the Eq instance for following datatypes.
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn a)
         (TisAn a') =
             a == a'

-- avoid use two integers. a1, b1 are instance definition
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b)
         (Two a' b') =
             a == a' && b == b'

-- avoid non exhaustive pattern matches (partial functions)
data StringOrInt =
    TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a)
         (TisAnInt a') = a == a'
    (==) (TisAString a)
         (TisAString a') = a == a'
    (==) _ _ = False

-- 4. two variables need to be the same type
data Pair a =
    Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a a')
         (Pair b b') = a == b && a' == b'

-- 5. two variable can be different types
data Tuple a b =
    Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a a')
         (Tuple b b') = a == b && a' == b'

-- 6.
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

-- 7.
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False

