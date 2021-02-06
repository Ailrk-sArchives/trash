{-# LANGUAGE DeriveGeneric        #-}

-- https://stackoverflow.com/questions/8633470/illegal-instance-declaration-when-declaring-instance-of-isstring
-- Usually instance is declared as instance TypeClass Type where
-- and Type can't be type synonym or something nested like (May Int)
-- This extension relax the restriction.
-- In short, FlexibleInstances = TypeSynonymInstances + nested type.
{-# LANGUAGE FlexibleInstances    #-}

-- [Char] is a specialization of [a], normally we can't write
-- typeclass instance for it.
-- Enabling to write type instance for type String = [Char]
{-# LANGUAGE TypeSynonymInstances #-}

module Ch5
  ( JValue
  , renderJValue
  , JSON(..)
  ) where


import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
import           Data.Text
import           Data.Vector
import           GHC.Generics


data JValue = JString Text
            | JNumber Double
            | JBool Bool
            | JObject (HM.HashMap Text JValue)
            | JArray (Vector JValue)
            | JNull
            deriving (Eq, Ord, Generic)

type JError = String

instance Show JValue where
  show = renderJValue

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" <> (pairs . HM.toList) o <> "}"
  where
    pairs [] = ""
    pairs ps = L.intercalate ", " (renderJValue <$> ps)
    renderJValue (k, v) = show k <> ": "  <> show v

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JError a


-- Implement typeclass instances whenver we see fits.
instance JSON Double where
  toJValue = JNumber
  fromJValue (JNumber n) = Right $ n
  fromJValue _           = Left "not a value"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue (JNumber n) = Right $ round n
  fromJValue _           = Left "not a value"

instance JSON String where
  toJValue = JString . pack
  fromJValue (JString s) = Right $ unpack s
  fromJValue _           = Left "not a value"


-- Overlapping instances means given same type
-- GHC has multiple possible instance to choose.
-- By defeault this is not allowed because it make type
-- system undecidable.
-- Workaround with TypeSynonymInstances.
