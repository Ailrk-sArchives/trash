{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}


module Types.TypeclassMetaprogramming where
import qualified Data.Map               as Map
import qualified Data.Vector            as Vector
import           Data.Void
import           Numeric.Natural

import           Control.Exception.Base (assert)
import System.IO

-- https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/

-------------------------------------------------------------------------------
-- typeclass as function from type to term
-- use compile time information to generate runtime behavior.

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
data Nat = Z | S Nat

class ReifyNat (a :: Nat) where
  reifyNat :: Natural

instance ReifyNat 'Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat ('S a) where
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

instance {-# OVERLAPPING #-} GNumFields () where
  gnumFields _ = 0

instance GNumFields a where
  gnumFields _ = 1

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b

authGenerialize :: Authentication -> Either (Username, Password) PublicKey
authGenerialize (AuthBaisc u p) = Left (u, p)
authGenerialize (AuthSSH p)     = Right p

numFieldsAuth :: Authentication -> Natural
numFieldsAuth = gnumFields . authGenerialize

n4 = numFieldsAuth (AuthSSH "asd")
n5 = numFieldsAuth (AuthBaisc "asd" "asd")

-------------------------------------------------------------------------------
-- Define a generic  NumFields that works on any types.
-- The idea is to convert a type to a isomorphic representation that we can
-- work with.
-- If we have typeclass that works on the transformed representation, we can
-- do various things with the new representation and map it back when we
-- are finished

class Generik1 a where
  type Rep1 a
  genericize1 :: a -> Rep1 a

-- Rep maps the type to it's representation type.
-- in our case we want to map it to either or tuple so we can count with GNumFields.
instance Generik1 Authentication where
  type Rep1 Authentication = Either (Username, Password) PublicKey
  genericize1 (AuthBaisc user pass) = Left (user, pass)
  genericize1 (AuthSSH key)         = Right key

-- we want (Rep a) available for GNumFields
numFields :: (Generik1 a, GNumFields (Rep1 a)) => a -> Natural
numFields = gnumFields . genericize1

n6 = numFields (AuthBaisc "asd" "asd")
n7 = numFields (AuthSSH "asd")

-------------------------------------------------------------------------------
-- improve generic
-- 1. this type will give wrong numfields because Rep a will be two nested
--    either, one is only a representation for the sum type.
--    To solve this, we can wrap each leaf with a new type Leaf.
data Foo = A (Either Int String) | B (Char, Bool)
data Boool = FF | TT
newtype Leaf a = Leaf { getLeaf :: a }

class Generik2 a where
  type Rep2 a
  genericize2 :: a -> Rep2 a

instance Generik2 Authentication where
  type Rep2 Authentication = Either (Leaf Username, Leaf Password) (Leaf PublicKey)
  genericize2 (AuthBaisc user pass) = Left (Leaf user, Leaf pass)
  genericize2 (AuthSSH key) = Right (Leaf key)

instance Generik2 Foo where
  type Rep2 Foo = Either (Leaf (Either Int String)) (Leaf (Char, Bool))
  genericize2 (A x) = Left (Leaf x)
  genericize2 (B x) = Right (Leaf x)

instance Generik2 Boool where
  type Rep2 Boool = Either () ()
  genericize2 FF = Left ()
  genericize2 TT = Right ()

numFields2 :: Generik2 a => GNumFields (Rep2 a) => a -> Natural
numFields2 = gnumFields . genericize2

n8 = numFields2 (A (Left 1))
n9 = numFields2 (B ('a', True))
n10 = numFields2 (AuthBaisc "asd" "asd")
n11 = numFields2 TT

------------------------------------------------------------------------------
-- dependent types

-- Note: a has kind *, so it will accpet whatever types. If the pattern
-- matching fails, it will get stuck.
-- For example, BadNot Char will return BadNot Char instead of evaluate further
--
-- DataKind promotes types to new kinds and constructors to types, which give
-- us more elements to work with at the type level.
type family BadNot a where
  BadNot 'True = 'False
  BadNot 'False = 'True

type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

------------------------------------------------------------------------------
-- runtime information -> compile time type
-- GADT and proof terms

data WhatIsIt a where
  ABool :: WhatIsIt Bool
  AInt :: WhatIsIt Int

-- the type of x is determined by tye constructor.
-- which constructor is passed is a runtime behavior. But we can infer the type
-- of the type parameter with GDAT
--
-- Think GADT as a proof of type equalities.
-- ABool => a ~ Bool
-- AInt => a ~ Int
doSometing :: WhatIsIt a -> a -> a
doSometing ABool x = not x
doSometing AInt x = x + 1

-- GADT with type level list.
infixr 5 `HCons`

-- hetergenous list
data HList as where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

n12 = True `HCons` "hellow" `HCons` 42 `HCons` HNil

------------------------------------------------------------------------------
-- proofs that works together.
data OneToThree a b c as where
  One :: OneToThree a b c '[a]
  Two :: OneToThree a b c '[a, b]
  Three :: OneToThree a b c '[a, b, c]

sumUpToThree :: OneToThree Int Int Int as -> HList as -> Int
sumUpToThree One (x `HCons` HNil) = x
sumUpToThree Two (x `HCons` y `HCons` HNil) = x + y
sumUpToThree Three (x `HCons` y `HCons` z `HCons` HNil) = x + y + z

------------------------------------------------------------------------------
data Even as where
  EvenNil :: Even '[]
  EvenCons :: Even as -> Even (a ': b ': as)


------------------------------------------------------------------------------
run :: IO ()
run = do
  assert (n1 == 5) (return ())
  assert (n2 == [1..8]) (return ())
  assert (n3 == [1..4]) (return ())
  assert (n4 == 1) (return ())
  assert (n5 == 2) (return ())
  assert (n6 == 2) (return ())
  assert (n7 == 1) (return ())
  assert (n8 == 1) (return ())
  assert (n9 == 1) (return ())
  assert (n10 == 2) (return ())
  assert (n11 == 0) (return ())

xproc0 = openFile "" ReadMode
xproc1 = openFile "" ReadMode
xproc2 = openFile "" ReadMode
