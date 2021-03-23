{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Notes.Lang.GHCLangExtensions.OverloadedStrings where

-- text:       unicode
-- bytestring: ascii

import Data.String

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

import Data.Text.Encoding.Error as TE

import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO

import qualified Data.ByteString.Short.Internal as BSI

newtype Cat = Cat T.Text deriving (IsString, Show, Eq)

newtype Dog = Dog BSI.ShortByteString deriving (IsString, Show, Eq)

-- Overloaded works here
fluffy :: Cat
fluffy = "Fluffy"

soft :: Dog
soft = "Soft"

-- soft' :: String -> Dog  -- this gives an error for some reasons.
-- soft' name = Dog name

decodeUtf8' :: BS.ByteString -> Either TE.UnicodeException T.Text
decodeUtf8' = undefined

decodeUtf8With :: TE.OnDecodeError -> BS.ByteString -> T.Text
decodeUtf8With = undefined
