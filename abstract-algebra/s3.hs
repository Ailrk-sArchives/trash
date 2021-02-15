{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE LiberalTypeSynonyms #-}


-- a group has some elements
-- a group is a set, so it should be a type.

-- if you want to check whatt type does a type family evaluates to just
-- use :kind! to evaluate it in the repl.

class Monoid a => Group a where
  inv :: a -> a

data E1 :: *
data E2 :: *
data E3 :: *

type Compose f g x = f (g x)

type family Comp (a :: * -> *) (b :: * -> *) c where
  Comp a b c = a (b c)

type K a = Int

type family Id a :: * where
  Id E1 = E1
  Id E2 = E2
  Id E3 = E3

type family Tau a :: * where
  Tau E1 = E2
  Tau E2 = E1
  Tau E3 = E3

type family Tau' a :: * where
  Tau' E1 = E1
  Tau' E2 = E3
  Tau' E3 = E2

type family Tau'' a :: * where
  Tau'' E1 = E3
  Tau'' E2 = E2
  Tau'' E3 = E1

type family Segma a :: * where
  Segma E1 = E2
  Segma E2 = E3
  Segma E3 = E1

type family Segma' a :: * where
  Segma' E3 = E2
  Segma' E2 = E1
  Segma' E1 = E3
