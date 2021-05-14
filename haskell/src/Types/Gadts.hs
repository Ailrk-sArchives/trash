{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Types.Gadts where

import           GHC.TypeLits

{-@ Frist, what's the motivation?
 @-}
-- This is just a type alias, and it's also a type function.
-- X can only be used at the type level, which takes
-- a type parameter a and give us back a concrete type.

-- in c++ it might look like this:
-- template<tyename T> using X = Either<T, T>;
type X a = Either a a

{-@ Now we want the type function to be more powerful
    We want it to actually act like a function.

    like this:
        type T [a] = Set a
    Or this:
        type F Bool = Char
        type F String = Int
    Or even recursive and using other type functions.
        data TrueType
        data FalseType

        type F [a] = F a
        type F (Map a b) = F b
        type F a | IsSimple a = a

        -- if a type is defined then it's true ...
        -- (figuratively)
        type IsSimple Bool
             IsSimple Int
             IsSimple Double
    Or maybe multi value fucntion? (What's that?)
        type Colelction a = [a]
             Colelction a = (Set a)
             Colelction a = (Map a b)

 @-}
-- Now let's write stuffs above as ...typeclasses.
class IsSimple a
instance IsSimple Bool
instance IsSimple Int
instance IsSimple Double

class Collection a c
instance Collection a [a]

-- A type function that traverses to decide whether
-- it has Int somewhere.
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


{-@ So we see, all of these type functions are implemented in haskell.
    It's their syntax make it looks more like a constraint rather
    then function. If you try to interpret type class as type function,
    the function body seems to be reversed.
      instance (HasInt a) => HasInt [a]
    is
      type HasInt [a] = HasInt a

    Let's see, we now have type to type functions, data to data functions,
    what about type to data?
       data Maybe = Just a | Nothing
    Here we make a type to data translation with two possible results.

    Compare with type class and functions we can see data declaration is
    quite inflexible.  it supports recursion, support multiple statement and
    muiltiple results. (No pattern matching no guard...)
@-}


{-@ Generalized Abstract data type
    Now you can have pattern matching and constant at t he left hand size
    of data statement!
    Say you want to do this:
      data T String = D1 Int
           T Bool   = D2
           T [a]    = D3
    you need to use GADT and write this:
      data T a where
        D1 :: Int -> T String     -- match when return T String
        D2 :: T Bool              -- match when return T Bool
        D3 :: (a, a) -> T [a]     -- match when return T [a]
@-}
-- to implement an evaluator for sk combinator

-- without gdat we write like this
-- notice K', S' are really the same kind, they can
-- replace one with another.
data Term' = K' | S' | Term' :@ Term'
infixl 6 :@

-- you don't have any type checking what so ever
eval' (K' :@ x :@ _)      = x
eval' (S' :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval' x                   = x
-- we know for S and K they actually ahve a type, some
-- input should not be valid.
-- But because the primitive data declaration makes no
-- distinction between this two other than saying K' and
-- S' and two different Term', there is no constraint
-- on how to use them
-- for example, you can write this:
eval' (S' :@ _ :@ y)      = y
-- or this
eval' (K' :@ x)           = x
-- which is bad bad.


{-@ GADT based small step evaluator
    Comparing with the one above, data constructor's return type
    is specified explicitly.
    Here it really just a pattern matching mechanism. We are saying
    data Term (a -> b -> a) = K
    data Term ((a -> b -> c) -> (a -> b) -> a -> c) = S
    data Term a = a
    data Term b = Term (a -> b) -> Term a
 @-}
-- a full implementation of small step SK evalutor with GADT
-- Now with Gadt, constructor can carry more type information.
-- For the type checker if it sees K, it knows (a -> b -> a)
-- needs to follow, otherwise it won't type check.
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
