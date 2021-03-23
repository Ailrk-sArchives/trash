{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Moretypes where

data Price =
    Price Integer deriving (Eq, Show)

data Size =
    Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChanceUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
               deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

-- newtypes checks

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 10
instance TooMany Bool where
    tooMany n = not n

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)
newtype Cow =
    Cow Int deriving (Eq, Show, TooMany)
newtype Flip =
    Flip Bool deriving (Eq, Show, TooMany)

-- record syntax for product type
data Person = MkPerson String Int deriving (Eq, Show)
namae :: Person -> String
namae (MkPerson s _) = s

-- record syntax allows named record fields.
data Homo =
    Homo { name :: String
         , age :: Int }
         deriving (Eq, Show)

-- algebra of datatypes

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
-- this is not in normal form
-- data Author = Author (AuthorName, BookType)

data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

-- sum of product, normal form.
-- data Expr =
--     Number Int
--   | Add Expr Expr
--   | Minus Expr
--   | Mult Expr Expr
--   | Divide Expr Expr

-- a stircter represenation for normal form of Expr
type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

type Expr =
    Either Number
        (Either Add
            (Either Minus
                Either Mult Divide))

data FlowerType =
    Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving Show

type Gardener = String

data Garden =
    Garden Gardener FlowerType
    deriving Show

data NormalGarden =
    Gardienia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show



