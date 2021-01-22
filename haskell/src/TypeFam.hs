{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module TypeFam where

import           Data.Char   (ord)
import qualified Data.IntMap as M

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
  data  GMap Int v = GMapInt (M.IntMap v) deriving Show
  empty = GMapInt M.empty
  lookup k (GMapInt m) = M.lookup k m
  insert k v (GMapInt m) = GMapInt $ M.insert k v m

instance GMapKey Char where
  data GMap Char v = GMapChar (M.IntMap v) deriving Show
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
  cinsert a as = a:as

-- use top level type family
type family Thing c
class Container c where
  conempty :: c
  coninsert :: Thing c -> c -> c

type instance Thing [c] = c
instance Container [a] where
  conempty = []
  coninsert a as = a:as


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


-- constraint type families
--

