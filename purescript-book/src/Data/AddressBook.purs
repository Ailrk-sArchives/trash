module Data.AddressBook where

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

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", "
               <> entry.firstName <> ": "
               <> showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", "
                <> addr.city <> ", "
                <> addr.state

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

