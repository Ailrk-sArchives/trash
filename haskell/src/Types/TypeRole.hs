{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Types.TypeRole where

import Data.Coerce
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid

-- a new type is always coercible to it's underlying type.
newtype ZipList a = ZipList { unZipList :: [a] }

-- >>> coerce (123 :: Sum Int) :: Product Int
-- >>> coerce (ZipList [1, 2, 3] :: ZipList Int) :: [Int]
-- Product {getProduct = 123}
-- [1,2,3]

-- why extra map if Sum Int is just Int?
-- fmap Sum is essentially applying id at runtime.
slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

-- just coerce it.
fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

-- Coercible is actually an equivalent relation.

-- Type roles ensures safe coersions.
-- Three roles:
-- 1. nominal     a ~ b
-- 2. representational   a equals of it's newtype.
-- 3. phantom   two types're always phantomly equal to each other

-- e.g GADT & type families both have nominal type roles.

type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a = a

-- note for this type family,
-- Coercible a b => Coercible (IntToBool a) (IntToBool b) failes in case
-- a = Int.
-- So it can't be representational.

data BST v
  = Empty
  | Branch (BST v) (BST v)

-- explicitly define type role.
type role BST nominal

