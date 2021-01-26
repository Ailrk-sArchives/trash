{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}

{-# LANGUAGE RankNTypes       #-}

module Types.Kindness where

{-@ We have parametric polymorphism for types by
    default. But there is no polymorphic kind.
    To support that we need extension.
@-}

-- by defualt, Bo has kind Bo :: * -> *, implies a :: *
-- if we have polymorphic kind, a can be any kinds.
-- Say a :: * -> *, then we can have Bo :: a -> *
-- thue, Bo :: (* -> *) -> *
data Bo a = Bo a

-- in contrast, this type constraint the kind of a to be
-- * -> *, thus we always know the kind of a.
-- With that information we can now deduce kind of b :: *
data Bobo (a :: * -> *) b = Bobo (a b)


-- withl poly kind the kind is polymorphic by default
-- data T (m :: k -> *) (a :: k) = Mkt (m a)
-- this means a has polymorphic kind k.
data T m a = MkT (m a)

-- but if you add a kind signature, you can constrain
-- the kindness.
data T' m (a :: *) = MkT' (m a)

-- This means m is a type with kind (k -> *), so it
-- at least be (* -> *), of course also (* -> * -> *)
-- GHC can infer type kind a from the context (m a)
-- It knows
--   (m :: k -> *) (m a :: *)
--   -----------------------
--       (a :: k)
data T'' (m :: k -> *) a = MkT'' (m a)

{-@ A practical example @-}

data Tree z = Leaf | Branch (Tree z) (Tree z)
-- this works with poly kind because now
-- Tree is inferred with kind Tree (k :: * -> *)
-- FunnyTree :: * -> *

-- This can't becomea concrete type now...
type FunnyTree = Tree []

-- This can
type FunnyTreeA a = Tree [a]
type IntTreeA = FunnyTreeA Int
