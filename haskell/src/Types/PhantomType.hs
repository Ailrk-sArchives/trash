{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Types.PhantomType where

import           Data.String
import           Data.Text as Text
import           Data.Text.Conversions

-- encoding arbitray type level invaraints and get statically checked
newtype Id a = Id Text deriving Eq

data User
data Blog
data Post

fetchUser :: Monad m => Id User -> m User
fetchUser = undefined

fetchBlog :: Monad m => Id Blog -> m Blog
fetchBlog = undefined

instance Show (Id User) where
  show (Id txt) = "user #" <> unpack txt

instance Show (Id Blog) where
  show (Id txt) = "blog #" <> unpack txt

instance Show (Id Post) where
  show (Id txt) = "post #" <> unpack txt

-- note n1 == n2 is an error. We can coerce then to the same type then it will
-- work.
npost :: Id Post
npost = Id "as post"

nuser :: Id User
nuser = Id "as user"

nblog :: Id Blog
nblog = Id "as blog"

isValid :: IsString a => a -> Bool
isValid _ = True

instance FromText (Maybe (Id a)) where
  fromText str = if isValid str then Just (Id str) else Nothing

-- we can use stand alone deriving to pick different implmenetation for a
-- specific type.
deriving instance ToText (Id User)
deriving instance ToText (Id Post)

instance ToText (Id Blog) where
  toText (Id x) = Text.concat ["Blog: ", x]

-- text
testToText = [toText nblog, toText npost, toText nuser]

-- data kinds and kind signatures

data RegStatusd = Registered | Anon
newtype UserId (s :: RegStatusd) = UserId Text
