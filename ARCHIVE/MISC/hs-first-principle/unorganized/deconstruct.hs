module Deconstruct where

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show
data FarmerRec =
    FarmerRec { name :: Name
              , acres :: Acres
              , farmerType :: FarmerType } deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

isWheatFarmer :: FarmerRec -> Bool
isWheatFarmer farmer = case farmerType farmer of
                         WheatFarmer -> True
                         _ -> False


-- handle bottom for records
-- wrap the product case with bottom case into a new type
data Car = Car { make :: String
               , model :: String
               , year :: Integer } deriving (Eq, Show)

data Automobile = Null
                | Automobile Car
                deriving (Eq, Show)

-- define list
data List a = Nil | Cons a (List a) deriving (Eq, Show)




