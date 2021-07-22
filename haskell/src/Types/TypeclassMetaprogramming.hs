{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Types.TypeclassMetaprogramming where
import qualified Data.Map               as Map
import qualified Data.Vector            as Vector
import           Data.Void
import           Numeric.Natural

import           Control.Exception.Base (assert)

-- https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/

-------------------------------------------------------------------------------
-- typeclass as function from type to term

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"

-- type application allows us to define TypeOf for void.
instance TypeOf Void where
  typeOf = "Void"

instance TypeOf a => TypeOf [a] where
  typeOf = "[" ++ typeOf @a ++"]"

-------------------------------------------------------------------------------
-- type level interpreter
data Z
data S a

class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + (reifyNat @a)

-------------------------------------------------------------------------------
-- overlapping instances.
-- this works somehow like template specialization in c++.
-- when there is overlapping instances, it choose the more specific instance

class IsUnit a where
  isUnit :: Bool

instance {-# OVERLAPPING #-} IsUnit () where
  isUnit = True

instance IsUnit a where
  isUnit = False

guardUnit :: forall a . IsUnit a => a -> Either String a
guardUnit x = case isUnit @a of
                True  -> Left "No unit"
                False -> Right x

-- Note this doesnt work because the instance needs to be picked at the compile
-- time, but we don't know which one to pick because type of a is instantiated
-- at runtime.
-- guardUnit :: forall a . a -> Either String a
-- guardUnit x = case isUnit @a of
--                 True -> Left "no unit"
--                 False -> Right x


-------------------------------------------------------------------------------
-- type families, function from type to type.

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)

n1 = reifyNat @(Sum (S (S Z)) (S (S (S Z))))

-------------------------------------------------------------------------------
-- e.g flattern
-- concat with arbitrary depth.

type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

class Flatten a where
  flatten :: a -> [ElementOf a]

-- base case
instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat


n2 = flatten [[[1 :: Integer, 2], [3, 4]], [[5, 6], [7, 8]]]

-------------------------------------------------------------------------------
-- type class as compile type code generation.
-- in this case it works like templates.

-- flatten @[[Int]] ->  id . concat . concat
n3 = flatten @[[Int]] $ [[1, 2], [3, 4]]

-------------------------------------------------------------------------------
-- open type family written within type class.

class HasKey a where
  type Key a
  hasKey :: Key a -> a -> Bool

instance HasKey (Vector.Vector a) where
  type Key (Vector.Vector a) = Int
  hasKey i vs = i >= 9 && i < Vector.length vs

instance Ord k => HasKey (Map.Map k v) where
  type Key (Map.Map k v) = k
  hasKey = Map.member

-------------------------------------------------------------------------------
-- datatype generic programming

type Username = String
type Password = String
type PublicKey = String

-- Sum type version
data Authentication = AuthBaisc Username Password
                    | AuthSSH PublicKey

class GNumFields a where
  gnumFields :: a -> Natural

instance GNumFields a where
  gnumFields _ = 1

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b

authToEither :: Authentication -> Either (Username, Password) PublicKey
authToEither (AuthBaisc u p) = Left (u, p)
authToEither (AuthSSH p)     = Right p

numFieldsAuth :: Authentication -> Natural
numFieldsAuth = gnumFields . authToEither

n4 = numFieldsAuth (AuthSSH "asd")
n5 = numFieldsAuth (AuthBaisc "asd" "asd")

-------------------------------------------------------------------------------
-- Define a generic  NumFields that works on any types.

class Generik a where
  type Rep a
  genericize :: a -> Rep a

instance Generik Authentication where
  type Rep Authentication = Either (Username, Password) PublicKey
  genericize (AuthBaisc user pass) = Left (user, pass)
  genericize (AuthSSH key)         = Right key

numFields :: (Generik a, Generik (Rep a)) => a -> Natural
numFields = gnumFields . genericize



-------------------------------------------------------------------------------
run :: IO ()
run = do
  assert (n1 == 5) (return ())
  assert (n2 == [1..8]) (return ())
  assert (n3 == [1..4]) (return ())
  assert (n4 == 1) (return ())
  assert (n5 == 2) (return ())
