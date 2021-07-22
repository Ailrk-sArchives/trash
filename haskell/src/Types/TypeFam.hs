{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Types.TypeFam where

import           Control.Monad.Except
import           Data.Char            (ord)
import qualified Data.IntMap          as M
import           Data.Maybe

import           Data.Kind
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits

import           Control.Monad.Trans



-- how you normall define a list with gadt??
data GList a where
  GCons :: !Char -> (GList Char) -> GList Char
  GListUnit :: !Int -> GList ()

{-@ Type families (indexed type families)
    Haskell's ad-hoc overloading of data types.
    family to overload data is class to overlod function.
@-}

{-@ toplevel data family  @-}

-- here we defined a data family XList.
data family XList a :: *

-- and here we created two overloads.
-- XList CHar and XList () are really two different types if you look
-- at their underlying value. But at the type level they are both
-- XList.
data instance XList Char = XCons !Char !(XList Char) | XNil

data instance XList () = XListUnit !Int

{-@ More example with the SK evaluator defined previously. @-}
data TermGADT a where
  K' :: TermGADT (a -> b -> a)
  S' :: TermGADT ((a -> b -> c) -> (a -> b) -> a -> c)
  (:@) :: TermGADT (a -> b) -> TermGADT a -> TermGADT b

infixl 6 :@

evalgadt :: TermGADT a -> TermGADT a
evalgadt (K' :@ x :@ _)      = x
evalgadt (S' :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
evalgadt x                   = x

-- you can't realy simulate this gadt though, because data family implies
-- that data instances has the same arity.
data family Fam a :: *

data instance Fam () = FamA Int Int
data instance Fam Int = FamB Char
data instance Fam (a -> b) = FamC (a -> b)

-----------------------------------------------------------------------

{-@ Associated (data familiy).
    we can give the assiciated data type a kind signature.
    each instance can have it's own data type.
@-}
-- let's make a generialzed trie
class GMapKey k where
  data GMap k :: * -> *
  empty :: GMap k v
  lookup :: k -> GMap k v -> Maybe v
  insert :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v = GMapInt (M.IntMap v) deriving (Show)
  empty = GMapInt M.empty
  lookup k (GMapInt m) = M.lookup k m
  insert k v (GMapInt m) = GMapInt $ M.insert k v m

instance GMapKey Char where
  data GMap Char v = GMapChar (M.IntMap v) deriving (Show)
  empty = GMapChar M.empty
  lookup k (GMapChar m) = M.lookup (ord k) m
  insert k v (GMapChar m) = GMapChar $ M.insert (ord k) v m

m :: GMap Char Int
m = insert 'a' 10 empty

n :: GMap Int Int
n = insert (10 :: Int) 10 empty

{-@ type family  is similar @-}

-- another example with data family
-- here we uses the type family instead of data family
class Collects c where
  type Elem c :: *
  cempty :: c
  cinsert :: Elem c -> c -> c

instance Collects [a] where
  type Elem [a] = a
  cempty = []
  cinsert a as = a : as

-- use top level type family
type family Thing c

class Container c where
  conempty :: c
  coninsert :: Thing c -> c -> c

type instance Thing [c] = c

instance Container [a] where
  conempty = []
  coninsert a as = a : as

{-@ Do some type level operations
    It's here again, but let's do some peano number
 @-}
data Zero
data Succ n
type family Pred' n
type instance Pred' (Succ n) = n
type instance Pred' Zero = Zero

-- you can write type family in one place.
-- this starts to feels like Gadt
type family Pred n where
  Pred (Succ n) = n
  Pred n = n

-- ambigious type
data KebabState = Kebab | NoKebab | NoMoreKebab deriving (Show, Eq)

class HasKebab a where
  type KGame a :: *
  hasKebab :: KGame a -> a

{-@ hasKebab :: KGame a -> KebabState @-}
-- this will cause a can't deduce Kebab a warning.
-- Imagine writing typeclass without type family, of course you need to
-- have the type parameter a some where to indicate which oveload to use.

class HasKebab' a where
  hasKebab' :: a -> a

-- some convenient language extensions
{-@ type applications
    Allows you to explicitly declare what types should be instantiated
    for argument of a function application.
    There are cases that you need to specify the type of a simple parameter,
    but to annotate it you needs to provides the full type signatures.
    Type application helps you to annotate only one type to make it type check.
@-}

type family FTypeApp a

-- only have two overloads here.
type instance FTypeApp Char = Bool
type instance FTypeApp Bool = Bool

functionApp = l $ ord h
  where
    g :: FTypeApp a -> a
    g _ = undefined

    -- this can be inferred.
    f :: Char
    f = g True

    -- give the tyupe info here.
    -- h' = g False   -- this causes an error.
    h = g @Char False

    x :: Enum b => b -> b
    x = undefined
    l = x @Int -- here specify l with use the Int overload.



{-@ 1. closed type families @-}
-- some type level programing.
-- standalone kind signature / kind signature + type familiy + polykind to
-- write type level function as if it's normal function.
-- data kinds promote all term level values into type, so we can do all term level
-- computations at the type level.

-- this form is called a closed type family.
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- '[1, 2, 3, 4, 5, 6]
type ListType1 = Append '[1, 2, 3] '[4, 5, 6]

type Cons :: forall a. a -> [a] -> [a]
type family Cons a xs where
  Cons a '[] = '[a]
  Cons a xs = a ': xs

-- --
type Consed1 = Cons 1 '[1, 2, 3]
type ConsedList = Append (Cons 1 (Append '[1, 2, 3] '[3, 4, 5])) ListType1
-- --

type Take :: forall a. Nat -> [a] -> [a]
type family Take n xs where
  Take 0 xs = '[]
  Take n (x ': xs) = x ': Take (n - 1) xs

type Length :: forall a. [a] -> Nat
type family Length xs where
  Length '[] = 0
  Length (x ': xs) = 1 + (Length xs)

-- --
type Take3OnList = Take 3 ConsedList
type LengthOfList = Length ConsedList
type Take30OnList = Take 30 ConsedList
-- --


{-@ 2. open type families @-}
type Label :: Type -> Symbol   -- type level string
type family Label t where
  Label Double = "number"
  Label String = "string"

-- how do we get the label into term level?
label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

v1 = label @Double
v2 = label @String


{-@ 3. Type level pattern matching @-}
data Currency
  = USD
  | CND
  | JPY
  | RMB
  | EUR
  deriving (Show, Eq)


type family IsAmericanDollar (a :: Currency) where
  IsAmericanDollar 'USD = ()
  IsAmericanDollar a = TypeError ('Text "It's not American dollar")

type R1 = IsAmericanDollar 'USD
type R2 = IsAmericanDollar 'RMB


{-@ Conclusion

    1. Type family is the type level function.

    2. Haskell has some quirks on type level operator and type applications,
       these are not important to the type system, they are just notational
       convinences.

    3. Type family pattern match on type paramter thus breaks the parametricity.

    4. Parametricity means a function works on all types. If we pattern matches on
       a type paramter a and have different behaviors based on what a is, apparently
       a doesn't represent all types any more.

    5. If a type family returns the kind 'Constraint', it's just a type class constraint
       without corresponding term level functions.

    6. There are lots of type level facilities, like type level natural numbers, type level
       String (Symbol) etc.

       Facilities are from GHC.TypeLits.

    7. Proxy are dummy values, or a safer undefined.

    8. It's possible to project some type level values to term level. One possible senario
       is from Symbol to string.

    9. Type equality can be expressed with '~'.
       Another use of ~ is to bind a super long constraint with a name and refer to the name
       later.

    10.
 @-}
