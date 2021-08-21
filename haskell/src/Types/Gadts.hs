{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Types.Gadts where

import           Cat.Cat.Arrows   ((>>>))
import           Data.Kind
import           GHC.TypeLits

-- GADT bascially equivalent to shove a type equiality constraint in each
-- data consntructor.
-- When doing pattern maching type a can be inferred by constructor used.
data Expr a
  = (a ~ Int) => LitInt Int
  | (a ~ Bool) => LitBool Bool
  | (a ~ Int) => Add (Expr Int) (Expr Int)
  | (a ~ Bool) => Not (Expr Bool)
  | If (Expr Bool) (Expr a) (Expr a)

evalExpr :: Expr a -> a
evalExpr (LitInt a) = a
evalExpr (LitBool a) = a
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Not n) =  not (evalExpr n)
evalExpr (If n then' else') =
  if evalExpr n then evalExpr then' else evalExpr else'

-- >>> evalExpr (If (LitBool True) (LitInt 1) (LitInt 2))
-- 1

-- Another way to think about it is that the type Expr a is indexed by term
-- level values.
-- At runtime we have (LitInt 10), so a for Expr a becomes Int.

-------------------------------------------------------------------------------
-- one motivation for GADT is to build inductive type level structural with
-- term levle data.


-- type level list with gadt
-- note:
-- 1. ts has kind '[Type]. a HList can contain type of whatever.
-- 2. base case HNil infer the list to '[]
-- 3. cons operation :# introduce a new type paramter t, and at the
--    type level it will be consed on top of the type list ts
-- 4. end result is that the type level list mirror operation at term level.
-- 5. Not all HLists can be compaired, cause their ts can be entirely different
--    types. e.g '[Int] is not the same as '[Int, Double].
data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength (_ :# as) = 1 + hLength as

-- note this head is type safe, HList '[] is not allowed here.
hHead :: HList (t ': ts) -> t
hHead (a :# _) = a

showBoolAtSecondPosition :: HList '[_1, Bool, _2] -> String
showBoolAtSecondPosition (_ :# n :# _) = show n

-- >>> showBoolAtSecondPosition (1 :# True :# 3 :# HNil)
-- >>> showBoolAtSecondPosition (1 :# False :# (LitBool True) :# HNil)
-- "True"
-- "False"

--------------------------------------
-- A problem with GADT is that normal typeclass facility doesn't work every
-- well. We need two typeclass to define Eq for HList.

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

-- with type family

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance (All Show ts) => Show (HList ts) where
  show HNil      = "()"
  show (a :# as) =  "(" <> show a <> ", " <> show as <> ")"

-- >>> :kind! All Eq '[Int, Bool]
-- All Eq '[Int, Bool] :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))

-- it's tricker to implement typeclass has upper constraints like Ord to Eq.
-- e.g Ord (HList ts)
-- Because you need to prove
--  All Ord ts, (Eq (HList ts))
-- when patten maching and recurs you also need to prove:
--  (Eq (HList (t ': ts)))


-----------------------------------------------------------Look at this:
type X a = Either a a
-- This is just a type alias, and it's also a type function.
-- X can only be used at the type level, which takes
-- a type parameter a and give us back a concrete type.

-- in c++ it might look like this:
-- template<tyename T> using X = Either<T, T>;

-- type predicates return constraint.
-- typeclass is just type predicate + term level definitions.
class IsSimple a
instance IsSimple Bool
instance IsSimple Int
instance IsSimple Double

class Collection a c
instance Collection a [a]

-- A type function that traverses to decide whether it has Int somewhere.
class HasInt a
instance HasInt Int

-- think it as: HasInt [a] = HasInt a
instance (HasInt a) => HasInt [a]
instance (HasInt a) => HasInt (Maybe a)

-- replace a with b and res with t
class Replace t a b res
instance Replace t a a t  -- need flexible instance
instance Replace t a b res => Replace [t] a b [res]
instance Replace t a b res => Replace (Maybe t) a b (Maybe res)
instance Replace t a b t

-- to implement an evaluator for sk combinator

data Term' = K' | S' | Term' :@ Term'
infixl 6 :@

-- you don't have any type checking what so ever
eval' (K' :@ x :@ _)      = x
eval' (S' :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval' x                   = x
-- eval' (K' :@ x)           = x


{-@ GADT based small step evaluator
    Comparing with the one above, data constructor's return type
    is specified explicitly.
    It's just a pattern matching. We are saying
    data Term (a -> b -> a) = K
    data Term ((a -> b -> c) -> (a -> b) -> a -> c) = S
    data Term a = a
    data Term b = Term (a -> b) -> Term a
 @-}
-- a full implementation of small step SK evalutor with GADT
data Term x where
  K :: Term (a -> b -> a)
  S :: Term ((a -> b -> c) -> (a -> b) -> a -> c)

  -- simply apply the first arg to the second arg.
  (:$) :: Term (a -> b) -> Term a -> Term b

infixl 6 :$

eval :: Term a -> Term a
eval (K :$ x :$ _)      = x
eval (S :$ x :$ y :$ z) = x :$ z :$ (y :$ z)
eval x                  = x


{-@ An example with list
    Use Gadt to encode if a list is empty or nonemty
@-}

data List' a = Const' a | Nil'

data Empty
data NonEmpty
data List x y where
  Nil :: List a Empty
  Cons :: a -> List a b -> List a NonEmpty

-- again, here we encode some extra information in to the list
{-@ Now this won't type check.
    silly 0 = Nil
    silly 1 = Cons 1 Nil
    You see all the sudden you have this super strong constraint on
    list type. Nil and Const are essentially different types.
@-}

first :: List a NonEmpty -> a
first (Cons x _) = x

-- now you need to differentiate between two types of list.
-- but this fells fall back to a usual list implementation
-- with Maybe and None...
rest :: List a NonEmpty -> Either (List a Empty) (List a NonEmpty)
rest (Cons _ xs) = case xs of
                     c@(Cons _ _) -> Right c
                     _            -> Left Nil

{-@ using GADT to represent stack
@-}
data Z
data S n

data Stack n where
  SEmpty :: Stack Z
  SPush :: Stack n -> Stack (S n)
  SPop :: Stack (S n) -> Stack n

theStack = SPush . SPush . SPush $ SEmpty
theStack' = SPop . SPop $ theStack

-------------------------------------------------------------------------------
-- tie shoe example to use gadt to make illegal state unrepresentative.
-- https://morrowm.github.io/posts/2021-08-02-shoes.html

data ShoeState = Off | Untied | On deriving Show

data Shoe l r where
  OffLR :: Shoe Off Off
  PutOnL :: Shoe Off r -> Shoe Untied r
  PutOnR :: Shoe l Off -> Shoe l Untied
  TieL :: Shoe Untied r -> Shoe On r
  TieR :: Shoe l Untied  -> Shoe l On

deriving instance forall (l :: ShoeState) (r :: ShoeState) . Show (Shoe l r)

type TieShoeMethod = Shoe Off Off -> Shoe On On

rllr :: TieShoeMethod
rllr = PutOnR >>> PutOnL >>> TieL >>> TieR

rrll :: TieShoeMethod
rrll = PutOnR >>> TieR >>> PutOnL >>> TieL

-- >>> rllr OffLR
-- TieR (TieL (PutOnL (PutOnR OffLR)))

-- this doesn't work
-- don't :: TieShoeMethod
-- don't = TieL >>> TieR >>> PutOnL >>> PutOnL
-- don't = TieL >>> TieR >>> PutOnL >>> PutOnL
