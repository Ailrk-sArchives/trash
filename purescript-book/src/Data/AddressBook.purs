module Data.AddressBook
    ( Address(..)
    , Person(..)
    , PhoneNumber(..)
    , PhoneType(..)
    , address
    , person
    , phoneNumber
    , examplePerson
    ) where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)


type Entry =
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address =
    { street :: String
    , city :: String
    , state :: String
    }

type AddressBook = List Entry

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstname lastname = head <<< filter
    (\e -> e.firstName == firstname && e.lastName == lastname)

dub :: forall a. Semigroup a => a -> a -> a
dub a b = a <> b

infixr 0 dub as :<

--

data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone

type PhoneNumber = { phoneType :: PhoneType, phoneNumber :: String }

type Person =
    { firstname :: String
    , lastname :: String
    , address :: Address
    , phoneNumbers :: Array PhoneNumber
    }

address :: String -> String -> String -> Address
address s c st = { street: s, city: c, state: st }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber pt s = { phoneType: pt, phoneNumber: s }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person fname lname addr phoneNos =
    { firstname:fname
    , lastname: lname
    , address: addr
    , phoneNumbers: phoneNos
    }

examplePerson :: Person
examplePerson =
    person "John" "Doe"
    (address "123 Fake St." "FakeTown" "CA")
    [ phoneNumber HomePhone "555-555-5555"
    , phoneNumber CellPhone "555-555-1111"]

instance showAddress :: Show PhoneType where
    show HomePhone = "HomePhone"
    show WorkPhone = "WorkPhone"
    show CellPhone = "CellPhone"
    show OtherPhone = "OtherPhone"
