module Constructvalues where

data GuessWhat =
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
                  deriving (Eq, Show)

------------------------------------------------
newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

-- vanilla Farmhouse
data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

-- use Product with more than 2 values
data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- better way
type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)


------------------------------------------------
-- just a value here.
idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

-- Programmer
data OS =
    Linux
  | BSD
  | Mac
  | Win
  deriving (Eq, Show)

data PL =
    Haskell
  | Cpp
  | Python
  | CommonLisp
  deriving (Eq, Show)

data Programmer =
    Programmer { os :: OS
               , lang :: PL }
    deriving (Eq, Show)

allOs :: [OS]
allOs = [ Linux
        , BSD
        , Mac
        , Win ]

allLang :: [PL]
allLang = [ Haskell
          , Cpp
          , Python
          , CommonLisp ]

allProgrammer :: [Programmer]
allProgrammer = [Programmer os lang | os <- allOs, lang <- allLang]
