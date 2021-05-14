{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Types.TypeFam where

import           Control.Exception
import           Control.Monad.Except
import           Data.Char            (ord)
import qualified Data.IntMap          as M
import           Data.Maybe

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
