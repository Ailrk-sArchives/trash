module Ch6 where

-- 1.
data Person = Person String deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood ->  Mood
settleDown x =
    if x == Woot
       then Blah
    else x

-- 3.
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

-- given datatype delacaration
data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah =
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

-- anonymous functions
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x


-- pattern matching
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False


