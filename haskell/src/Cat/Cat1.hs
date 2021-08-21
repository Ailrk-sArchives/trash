{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
module Cat.Cat1 where

import           Data.Kind

-- natural transformation transforming one functor to another while maintain
-- the internal structure.
type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
data (~>) f g = NatTrans { unNatTrans :: forall x. f x -> g x }

type Const :: Type -> Type -> Type
data Const a b = Const { getConst :: a }

-- adjunction
type Adjunction :: (k -> k') -> (k' -> k)
                -> (k' -> k' -> Type) -> (k -> k -> Type)
                -> Constraint
class Adjunction f g p q | f -> g, g -> f where
  leftAdj :: (p (f a) b) -> (q a (g b))
  rightAdj :: (q a (g b)) -> (p (f a) b)

type Exists :: (Type -> Type) -> Type
data Exists f where
  Exists :: f x -> Exists f

-- adjunction triple Exists -| Const -| Forall
instance Adjunction Exists Const (->) (~>) where
  leftAdj :: (Exists f -> a) -> (f ~> Const a)
  leftAdj g = NatTrans $ \fx -> (Const (g (Exists fx)))

  rightAdj :: (f ~> Const a) -> (Exists f -> a)
  rightAdj g (Exists fx) = getConst (unNatTrans g fx)

