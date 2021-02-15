{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE RankNTypes #-}

-- https://stackoverflow.com/questions/4922560/why-doesnt-typesynonyminstances-allow-partially-applied-type-synonyms-to-be-use

-- encode boolean logic in lambda calculus
-- This
-- :kind! Not True
-- doesn't work because haskell doesn't allow partially application
-- of type synonym.
-- The reason is it's hard to decide whether two partially applied
-- synonysm are equal.
type True x y = x
type False x y = y
type Not b x y = b y x
type And b1 b2 x y = b1 (b2 x y) y
type Or b1 b2 x y = b1 x (b2 x y)
