module Valid where

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOk :: Age -> ValidatePerson Age
ageOk age = case age >= 0 of
              True -> Right age
              False -> Left [AgeTooLow]

nameOk :: Name -> ValidatePerson Name
nameOk name = case name /= "" of
              True -> Right name
              False -> Left [NameEmpty]

mkPerson name age =
    mkPerson' (nameOk name) (ageOk age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right nameGood) (Right ageGood) =
    Right (Person nameGood ageGood)
mkPerson' (Left badName) (Left badAge) =
    Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge









