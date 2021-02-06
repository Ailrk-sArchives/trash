{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

{-
   2020-09-19
   Report on Datatype-generic-programming
   http://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf
-}

module DatatypeGenericProgramming where

{- 1. Introduction
 - What is generic programming?
   So called polytypism. (type safe parameterization by a datatype)

 - Genericity and parameterization
   Genericity comes with parameterizations. being able to parameterize
   computation, we can reuse the same code without extra boilderplates.

   Genericity is relative to languages. Pass by value (parameterize by value)
   in C might be called generic programming for a assembly programmer;
   similarly, parametric polymorphism is generic in a java setting.
   For haskell generic programming means finding the same structures between
   different algebraic data types.
-}

{-------------------------------------------------------------------------
   - 2. Generic programming
   - Parameterization -
     From low level parameterization to higher level parameterization:
     - Parameterize by value is called pass by value:
         Your procedure can change it's behavior based on what parameters
         get passed in.

     - parameterize by function is called higher-order function;
         Think about cps, the continuation of the cmputation is determined
         by the function get passed in.
         Higher order fuction is still pass by value, but now it's a function
         value that can be applied to other data.
     - Parameterize by type is we normally called polymorphism;
         If you're writing C++, template<typename T> T add(T a, T b);
         parameterized the type T, so the concrete type of the function
         is determined by T passed in.
         // strictly speaking, in true polymorphic system, code only get
         // generated once, instead of being specialized multiple times
         // like c++. However c++'s monophormization approach has some
         // performance benefits.
     - Parameterize by shape
         Similar idea for parameterize by type as 'by function' to 'by value'.

   It's like building an isomorphism between different data types.
-}

-- without parameterization by type (parametric polymorphism) :(
data ListI a = NilI | ConsI Int (ListI Int)

data ListC a = NilC | ConsC Char (ListC Char)

-- with parameterize by type :)
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- some helper funr later use
headL (Cons x xs) = x
headL Nil = error "bad"

tailL (Cons x xs) = xs
tailL Nil = error "bad"

append :: List a -> List a -> List a
append xs Nil = xs
append xs (Cons y ys) = Cons y (append xs ys)

-- parameterize by functions
-- The most famous example should be generalizing recursing
-- with fold.
-- Check out these repeatative implementation
-- They all look like induction with base case and an inductive step.
map' :: (a -> b) -> List a -> List b
map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

sum' :: Num a => List a -> a
sum' Nil = 0
sum' (Cons x xs) = x + (sum' xs)

-- why not
foldl'' :: (b -> a -> b) -> b -> List a -> b
foldl'' _ base Nil = base
foldl'' f base (Cons x xs) = f (foldl'' f base xs) x

-- and just
map'' f xs = foldl'' (Cons . f) Nil xs

sum'' xs = foldl'' (+) 0 xs

-- much better.

{-
   C++ template is an example for genericity by structure
   The need arrives from working with abstract data types.
   For instance, in STL you pass iterators to different algorithms witout
   needing to worry about what you're passing. In the context of
   std::algorithm, iterator is the only thing you need to care about.
-}

{-
    Genericity by property
     An extension on top of genericity by structure, and including
     equational constraints.

     Here is another implementation of state monad. Put it here is to
     indicate Monad implementation demonstrates genericity by
     property. Monad is not only a common interface over different data types,
     the implementation need to conform monad rules. Namely left identity,
     right identity and associativity.
-}
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f ms = State $ \s ->
    let (a, s') = runState ms s
     in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State gs) <*> (State ms) = State $ \s ->
    let (f, s') = gs s
        (a, s'') = ms s'
     in (f a, s'')

instance Monad (State s) where
  return = pure
  (State ms) >>= f = State $ \s ->
    let (a, s') = ms s
     in (runState $ f a) s'

{-
   - Genericity by stage
     Genericity for metaprogramming. For instance, c++ template instantiate
     at compile time leads c++ program be executed in a two stage fashion.
     TemplateHaskell has the similar stagin concept.
-}

{-
   - 2.7 Genericity by shape
     Parameterization over different shapes. Say you have a tree and a list,
     and you can fold over both of them. But it's hard to write a generic
     fold to take both tree and list as input because this two algebraic data
     types have different shape.
     Data generic programming aim to parameterize shape of data type to
     make program works for types with different shapes.
-}

-- BTree and List above both have fold method, and they do very similar
-- thing. The reason we can't merge them together is because this two
-- algebraic data types have different shapes.
data BTree a = Tip a | Bin (BTree a) (BTree a)

-- with parmetric polymorphism this function can work for different types.
-- with higher order function the function application can be generalized.
foldB' :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldB' t b (Tip x) = t x
foldB' t b (Bin xs ys) = b (foldB' t b xs) (foldB' t b ys)

reflectB :: BTree a -> BTree a
reflectB = foldB' Tip nib
  where
    nib xs ys = Bin ys xs

-- one catch is this function cannot be merged with foldL even though
-- they are so similar.
-- The parameterization we need is a notion to accept different shape
-- yet perform the same computation.

-- Tool for us to achieve this is Bifunctor and Fix point,
-- fold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
-- The shape parmeter s varies, and it determines the shape of the input data.
-- Fix s is used to represent recursive datatypes.
-- For one instantiation Fix s a is isomorphic with List a, on the other
-- it's isomorphic with Tree a.
-- With bifunctor we can state the conherence between instances of fold for
-- different shapes.
-- discuss later.

{-
   - 2.8 Universal vs ad-hoc genericity

-}

{-
   - 2.9 Another dimension of clssification.

-}

{-------------------------------------------------------------------------
   - 3 Origami programming
   Data structure determine program strucuture.
-}

-- 3.1 map and fold on list
mapL :: (a -> b) -> (List a) -> (List b)
mapL f Nil = Nil
mapL f (Cons x xs) = (Cons (f x) (mapL f xs))

foldL :: b -> (a -> b -> b) -> List a -> b
foldL e f Nil = e
foldL e f (Cons x xs) = f x (foldL e f xs)

-- simple application
filterL :: (a -> Bool) -> List a -> List a
filterL p = foldL Nil (add p)
  where
    add p x xs = if p x then Cons x xs else xs

-- 3.2 unfolds on list
-- unfold is a dual to fold. It unpack a data structure into a list.
-- p is the predicate to decide when unfolding stops.
-- f convert the current value to a value in the final list.
-- g determine what the input of f will be in the next iteration.
-- x is the base.
unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g x =
  if p x
    then Nil
    else Cons (f x) (unfoldL p f g (g x))

-- use of unfold
preds :: Int -> List Int
preds = unfoldL (0 ==) id pred
  where
    pred n = n - 1

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p = unfoldL (firstNot p) headL tailL
  where
    firstNot p Nil = True
    firstNot p (Cons x xs) = not (p x)

-- 3.3 origami for binary trees.
data Tree a = Empty | Node a (Tree a) (Tree a)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f Empty = Empty
mapT f (Node x xs ys) = Node (f x) (mapT f xs) (mapT f ys)

-- fold tree into a value, again similar function as foldL, but
-- because List and Tree has different shapes we need to provde
-- another implementation
foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT e _ Empty = e
foldT e f (Node x xs ys) = f x (foldT e f xs) (foldT e f ys)

-- let's use foldT to collapse a tree into a list.
inorder :: Tree a -> List a
inorder = foldT Nil glue
  where
    glue x xs ys = append xs (Cons x ys)

unfoldT :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> b) -> b -> Tree a
unfoldT p f g h x
  | p x = Empty
  | otherwise = Node (f x) (unfoldT p f g h (g x)) (unfoldT p f g h (h x))

-- calkin wilf tree
-- side note:  formal definition
--       a/b
--      /   \
-- a/(a+b)  (a+b)/b
-- (const False) tells you the tree expand forever.
cwTree :: Tree Rational
cwTree = unfoldT (const False) frac left right (1, 1)
  where
    frac (m, n) = m / n
    left (m, n) = (m, m + n)
    right (m, n) = (m + n, n)

-- 3.4 Holymorphism (higher order polymorphic).
--
-- anamorphism (unfold) followed by catamorphism (fold) to avoid building
-- intermediate data structures.
-- (Metamorphism is the opposite, whereares, fold and unfold.)
--
-- Unfold followed by a fold is very common. It's like in producer-consumer
-- model, on one hand you have a producer of the foldable, on the other hand
-- the fold function consumer the foldable.
-- p f g are for unfold
--e h are for fold
hyloL :: (b -> Bool) -> (b -> a) -> (b -> b) -> c -> (a -> c -> c) -> b -> c
hyloL p f g e h = foldL e h . unfoldL p f g

-- deforestrated version. avoid intermeiary all together.
hyloL' :: (b -> Bool) -> (b -> a) -> (b -> b) -> c -> (a -> c -> c) -> b -> c
hyloL' p f g e h x = if p x then e else h (f x) (hyloL' p f g e h (g x))

fact :: Integer -> Integer
fact = hyloL (0 ==) id pred 1 (*)

-- Similar definition of hylomorphism for binary trees.
-- find a trend, the more generalize a function is, the more crazy the
-- type signature will be.
hyloT ::
  (b -> Bool) ->
  (b -> a) ->
  (b -> b) ->
  (b -> b) ->
  c ->
  (a -> c -> c -> c) ->
  b ->
  c
hyloT p f g1 g2 e h x =
  if p x
    then e
    else h (f x) (hyloT p f g1 g2 e h (g1 x)) (hyloT p f g1 g2 e h (g2 x))

qsort :: Ord a => List a -> List a
qsort = hyloT (== Nil) headL littles bigs Nil glue
  where
    glue x xs ys = append xs (Cons x ys)
    littles (Cons x xs) = filterL (<= x) xs
    bigs (Cons x xs) = filterL (> x) xs

-- 3.5 Short-cut fusion
-- Short cut fusion allows elimination of intermediate data structures
-- using rewrite rules that can also be performed automatically during
-- compilation.
--
-- Simplify the deforestation optimization. (build is easier for compiler
-- to work with).
buildL :: (forall b. b -> (a -> b -> b) -> b) -> List a
buildL g = g Nil Cons

-- use buildL to make unfoldL
-- unfoldL' :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
-- unfoldL' p f g b = buildL (h b)
--   where
--     h b n c = if p b then n else c (f b) (h (g b) n c)

-- use buildL to make reverse
reverseL xs = buildL (\n c -> foldL id (\x g -> g . c x) xs n)

buildT :: (forall b. b -> (a -> b -> b -> b) -> b) -> Tree a
buildT g = g Empty Node

-- 3.6 Datatype genericity

{-------------------------------------------------------------------------
   - Origami Patterns
     Since program structures is determined by data structures, it makes
     sense to abstract from the determining shape, leaving only what programs
     differet shape have in common.

-}

-- Let's look at fix as a function first.
-- this function repeat recursively apply f to another f.
-- Say fix (1:)
-- we get
-- let (x = (1:) x) in x
-- let (x = (1:) x) in (1:) x
-- let (x = (1:) x) in (1:) ((1:) x)
-- ...
fix :: (a -> a) -> a
fix f = let x = f x in x

-- For list and tree, they are both recursive data structure.
-- When we want to abstract over recursive data type, we use Fix
-- s: shape type. It's a higher kinded type
-- a: the data
data Fix (s :: * -> * -> *) a = In {out :: (s a (Fix s a))}

-- note another way to define fix
data Fix' (f :: *) = Fix' f (Fix' f)

-- For instance, for list
--   Fix ListF a
-- = ListF a (Fix ListF a)
-- = ListF a (ListF a (Fix ListF a))
-- ...

-- Let's use Fix to make list and tree.

data ListF s a = NilF | ConsF s a

data TreeF s a = EmptyF | NodeF s a a

data BTreeF s a = TipF s | BinF s a a

type ListFix a = Fix ListF a

type TreeFix a = Fix TreeF a

type BTreeFix a = Fix BTreeF a

-- 3.7 bifunctors
class Bifunctor s where
  bimap :: (a -> c) -> (b -> d) -> (s a b -> s c d)

{-------------------------------------------------------------------------
   - Essense of the iterator pattern

-}

{-------------------------------------------------------------------------
   - Conclusion

-}
