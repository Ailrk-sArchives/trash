module Tys

import Data.Vect

------------------------------------------------------------------------------
-- idris allows overloaded constructor names.

-- Some idris sugars (pure synatic transformation):
-- cons list: [] -> Nil, [1, 2, 3] -> 1 :: 2 :: 3 :: Nil
-- snoc list: [<] -> Lin, [< 1, 2, 3] -> Lin :< 1 :< 2 :< 3

------------------------------------------------------------------------------
-- maybe has two paramter being lazy. Only one will eventually be used.
fromJust : Maybe a -> a
fromJust = maybe (?fromJustTakesNothing) id

-- (,) is sugar for MkPair a b : Pair a b
fred : (String, Int)
fred = ("Fred", 42)

-- (,,) is just a syntatic sugar for nested tuples.
jim : (String, Int, String)
jim = ("Jim", 25, "Cambridge")

------------------------------------------------------------------------------
-- dependent paris

-- some dependent type:
--  universal quantification is represented as dependent function:
--      forall (x : A). B(x), type B(n) is indexed by x : A.
--      so it's universal type depends on value. (PI Type)
--  existential quanfitication is represented as dependent pair:
--      exists (x:A).B(x), we know B(n) has a x:A, but we don't know exactly
--      what it is. It's exitential type depends on value. (Sigma Type)

-- Pi type is easy to represent, we only need funtion that takes a value that
-- return a type.
-- Sigma type need a 2-tuple that the type of the second element depends on the
-- value of the first.

data DPair' : (a : Type) -> (p : a -> Type) -> Type where
  MkDPair' : {p : a -> Type} -> (x : a) -> p x -> DPair' a p

-- specifying the n with the value in fst tuple.
vecDP : (n : Nat ** Vect n Int)
vecDP = (2 ** [3, 4])

vecDP' : DPair Nat (\n => Vect n Int)
vecDP' = MkDPair 2 [3, 4]

-- Here we say there exists a p such that Vec p a, but we don't really know
-- what p is, neither do we care because we only need a vector with the right
-- length after we filter out some elements.

filter' : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter' p Nil = (_ ** [])
filter' p (x :: xs)
    = case filter' p xs of
           (_ ** xs') => if p x then (_ ** x :: xs')
                                else (_ ** xs')

------------------------------------------------------------------------------
record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

main : IO ()
main = putStrLn "good"
