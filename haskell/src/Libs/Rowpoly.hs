{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators #-}
module Libs.Rowpoly where

import           Control.Applicative
import           Control.Lens        hiding (Identity)
import           Control.Lens.TH
import           Data.Char
import           Data.Kind
import           Data.Maybe
import           Data.Singletons
import           Data.Vinyl
import           Data.Vinyl.Functor

data Fields = Name | Age | Sleeping | Master deriving Show
type LifeForm = [Name, Age, Sleeping]

type family ElF (f :: Fields) :: Type where
  ElF Name = String
  ElF Age = Int
  ElF Sleeping = Bool
  ElF Master = Rec Attr LifeForm

newtype Attr f = Attr { _unAttr :: ElF f }

makeLenses ''Attr

instance Show (Attr Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr Sleeping) where show (Attr x) = "sleeping: " ++ show x
instance Show (Attr Master) where show (Attr x) = "master: " ++ show x

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

data S f where
  SName :: S Name
  SAge :: S Age
  SSleeping:: S Sleeping
  SMaster :: S Master

-- ok this is your row inistialization...
jon = (SName =:: "jon")
   :& (SAge =:: 23)
   :& (SSleeping =:: False)
   :& RNil

dog1 = (SName =:: "dog1")
    :& (SAge =:: 9)
    :& (SSleeping =:: True)
    :& (SMaster =:: jon)
    :& RNil

-- wakeup :: (Sleeping `RecElemFCtx` fields) => Rec Attr fields -> Rec Attr fields

wakeup = rput $ SSleeping =:: False
fallasleep = rput $ SSleeping =:: True

dog2 = wakeup dog1
dog3 = fallasleep dog2

dog4 = dog3 ^. rlens @Sleeping
