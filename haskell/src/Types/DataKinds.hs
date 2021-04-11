{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types.DataKinds where

import           Data.Proxy

data TypeList l where
  Nil ::TypeList '[]
  (:::) ::a -> TypeList l -> TypeList (a ': l)

infixr :::

-- >>> 1 ::: "Hello" ::: 1.1 :: TypeList '[Int, String, Double]

type family Param f :: [*] where
  Param (a -> f) = a ': Param f
  Param r = '[]

type  family Result f :: * where
  Result (a -> f) = Result f
  Result r = r

class (Param f ~ l, Result f ~ r) => ToTypeList f l r where
  translate :: f -> TypeList l -> r

instance (ToTypeList f l r) => ToTypeList (a -> f) (a ': l) r where
  translate f (a ::: l) = translate (f a) l

instance (Param f ~ '[], Result f ~ r, f ~ r) => ToTypeList f '[] r where
  translate r Nil = r
