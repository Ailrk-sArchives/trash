{-# LANGUAGE ExistentialQuantification #-}
module PL.DenotationalSem where
-- just some notes
-- denotational semantics maps haskell problem to their mathematically meaning.

-- being a purely function language makes this easier to do than imperative language,
-- as the only corner cases is to define recursive funtions.
-- Typically for imperative langauges, denotational semantics always involve some operational
-- aspects in it.

-- We have a notation for it: [[ ]]
-- e.g
-- code goes in [[]], everything outside is math.
--  [[ 2 * 5 ]] = 10
--
-- denotational semantics are compostitional:
--  [[ a + b ]] = [[a]] + [[b]]

{-@ barber's paradox and impredicative definition @-}
shaves :: Integer -> Integer -> Bool
1 `shaves` 1 = True
2 `shaves` 2 = False
0 `shaves` x = not (x `shaves` x)
_ `shaves` _ = False

-- what is this?
-- Denotational semantics must work with partial function. For shaves, domain is clearly
-- not defined for 0 0, since return type is Bool but we can never get it.
-- it doesn't halt.
_ = 0 `shaves` 0

{-@ bottom type _|_ @-}
-- as we just saw, although function shaves should return a Bool, and it is indeed well typed,
-- for input 0 0 it will never halt and we can never get the bool it promise.
-- As long as there is way to create self reference struture, every function that promise to
-- return something has the chance to fail that claim.
--
-- Thus, a type a -> Bool is not really only return two possible values True and False, there is
-- the third possiblilty that it doesn't get the result at all.
--
-- However we are doing denotational semantics for programming language, the goal is to being able
-- to define the semantics in terms of mathematical structure.
-- How do we handle this?
--
-- We add a new value called the bottom for every types in the type system.
-- A bool is not really data Bool = True | False, but rather True | False | _|_
-- The same for integer, denotationally we have |Z| + 1 elements.
--
-- Because undefined inhabited under every types, you can treat it as any type you like.
--
-- to create a instance of bottom in haskell, you use undefined. It works with type inference and
-- the type checker can work out by itself what type the bottom value inhabitate.
--
-- sides: it follows from Curry Howard isomorphism that any value of polymorphic forall a. a must
-- denote _|_
_ = undefined


