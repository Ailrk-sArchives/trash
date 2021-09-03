module Dt1

-- There is no need for type family, ordinary funtions will do already.

-- function that returns a Type
isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

mkSingle : (x : Bool) -> isSingleton x
mkSingle True = 0
mkSingle False = []

-- if isSingleton we use x directly. Otherwise just normal recursive definition
-- for sums.
sums : (single : Bool) -> isSingleton single -> Nat
sums True x = x
sums False [] = 0
sums False (x :: xs) = x + sums False xs

-- this defines a family of types.
data Vec : Nat -> Type -> Type where
  Nil : Vec Z a
  (::) : a -> Vec k a -> Vec (S k) a  -- (::) can be overloaded

(++) : Vec n a -> Vec m a -> Vec (n + m) a
(++) Nil ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

-- Well, this also defines a family of types.
isNotSingleton : Bool -> Type
isNotSingleton True = List Nat
isNotSingleton False = Nat

-- They're all family of types indexed by some arguments.
-- it's a Finite set indexed by Nat.
data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

-- Fin is just a non zero Nat. We can use it to index vector.

-- note you can pass implicit parameters to indicate type of a and n.
-- It's the same as saying for a to be true it needs to be a proposition first.
index : {a : Type} -> {n : Nat} -> Fin n -> Vec n a -> a
index FZ (x::xs) = x
index (FS k) (x::xs) = index k xs

-- You can pass implicit arguments explicitly. Type application is essentially
-- this.

index2OfVec3 : List Int
index2OfVec3 = index {a=Int} {n=3} FZ vec :: Nil
  where
    vec : Vec 3 Int
    vec = 1 :: 2 :: 3 :: Nil

------------------------------------------------------------------------------

main : IO ()
main = do
  let v3 = index2OfVec3
  putStrLn (show v3)
