module Dt1

-- There is no need for type family, ordinary funtions will do already.

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- They're all family of types indexed by some arguments.
-- it's a Finite set indexed by Nat.
data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

-- Fin is just a non zero Nat. We can use it to index vector.

-- note you can pass implicit parameters to indicate type of a and n.
-- It's the same as saying for a to be true it needs to be a proposition first.

-- any arguments can be given a name
index : {a : Type} -> {n : Nat} -> (i:Fin n) -> (xs:Vec n a) -> a
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

data IsElem : a -> Vec n a -> Type where
  Here : {x:a} -> {xs:Vec n a} -> IsElem x (x :: xs)
  There : {x, y:a} -> {xs:Vec n a} -> IsElem x xs -> IsElem x (y :: xs)

-- type as proposition and term is the proof.
inVec : IsElem 5 (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)
inVec = There (There (There (There Here)))

------------------------------------------------------------------------------
-- mutual recursion
-- for dependent types functions and data types must be defined before use,
-- because dt allows function to be part of the type, and decidable type
-- is necessary for type checking.
-- Totality checker will checker potential non dedicable functions and report
-- an error (the checker knows a function terminates if it's parameter is
-- well founded and decreasing in each step, because well founded set has no
-- infinite decreasing chain. Other use of recursions can never halt)

-- Functions compiled differently in mutal block. Type delarations are added
-- first, then function body. This way no function types will depends on
-- reduction of function body.
mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

------------------------------------------------------------------------------
-- IO
greet : IO ()
greet = do
  putStr "What's your name? "
  name <- getLine
  putStrLn ("hello, " ++ name)

------------------------------------------------------------------------------
-- Lazy

-- Lazy value can be wrapped in type Lazy. The forcing if enforced by type.
-- e.g An implicit coercion from Lazy a to a will force an evaluation.
ifThenElseYeet : Bool -> Lazy a -> Lazy a -> a
ifThenElseYeet True t _ = t
ifThenElseYeet False _ e = e

------------------------------------------------------------------------------
-- codata for infinite data structures.
-- data are as it is, codata are computation that you want to pass around.
-- For a type T, To have infinite data type, we need to make T lazy,

-- In idris there where codata as marker to transform T to Inf T, but in
-- idris2 you do it mannually.

data Streamy : Type -> Type where
  ConStream : (e : a) -> Inf (Streamy a) -> Streamy a

ones : Streamy Nat
ones = 1 `ConStream`  ones

main : IO ()
main = do
  let v3 = index2OfVec3
  putStrLn (show v3)
