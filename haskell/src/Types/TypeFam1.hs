{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}


module Types.TypeFam1 where

import           Data.Kind
import           Data.Word
import           GHC.TypeLits

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Proxy

-- type family is heavily used in vector library.

------------------------------------------------------ ------------------------
{-@ typeclass are type level predicates @-}
data Defined e = Yes | No e

type family And (a :: Defined Symbol) (b :: Defined Symbol) :: Defined Symbol where
  And 'Yes 'Yes = 'Yes
  And 'Yes e = e
  And e 'Yes = e
  And e1 e2 = e1

type family IsEq (a :: *) :: Defined Symbol
type instance IsEq Int = Yes
type instance IsEq [a] = IsEq a
type instance IsEq (a, b) = And (IsEq a) (IsEq b)

type Eq' a = IsEq a ~ Yes

all' :: Eq' a => [a] -> Bool
all' = undefined

none' :: forall a . IsEq a ~ Yes  => [a] -> Bool
none' = undefined

a1 = all' [1 :: Int, 2]
-- this doesn't type check
-- b = all' ['a', 'b']

-- works the same
a2 = none' [1 :: Int, 2]

------------------------------------------------------ ------------------------
{-@ disjoints @-}
data IntegralOrFactional = INTEGRAL | FRACTIONAL | NONE

type family IsIntegralOrFractional t :: IntegralOrFactional

type instance IsIntegralOrFractional Int = INTEGRAL
type instance IsIntegralOrFractional Integer = INTEGRAL
type instance IsIntegralOrFractional Double = FRACTIONAL

type family IsIntegral' (c :: IntegralOrFactional) :: Defined Symbol where
  IsIntegral' INTEGRAL = Yes
  IsIntegral' FRACTIONAL = No "Not integral"
  IsIntegral' c = No ""


type IsIntegral t = IsIntegral' (IsIntegralOrFractional t)

type Tt1 = IsIntegral Int ~ Yes

foo1 :: IsIntegral t ~ Yes => t -> t
foo1 = id

a3 = foo1 (1 :: Int)

------------------------------------------------------ ------------------------
-- use constraint
-- note this is very different from a simple type level funtion.
-- It's used as a type level predicate that requires a constructive proof.
-- Here we define (Ord' Int) = (), means the return value of Ord' Int is an
-- elemnet in Constraint. It doesn't matter what it is becasue the only thing
-- that is important is it exists.
--
-- Constraints can only be used in the context, we can also use type classes
-- as type families that return a Constraint.

type family Ord' (a :: *) :: Constraint
type instance Ord' Int = ()
type instance Ord' Char = ()
type instance Ord' (a, b) = (Ord' a, Ord' b)

gt :: Ord' a => a -> a -> a
gt = undefined


------------------------------------------------------ ------------------------
-- use type error

type family IsInt (a :: Type) :: Type where
  IsInt Int = ()
  IsInt a = TypeError (Text "type " :<>: ShowType a :<>: Text " is not Int" )

type family ByteSize x where
  ByteSize Word16 = 2
  ByteSize Word8 = 1
  ByteSize a = TypeError (Text "the type " :<>: ShowType a :<>: Text " is not exportable")

-- wow. Type level values get into term.
bytesOfWord16 = natVal (Proxy :: Proxy (ByteSize Word16))


------------------------------------------------------ ------------------------
-- Constraint synonym
-- Now this constraint can be use everywhere else.
type StackM (r :: *) (m :: * -> *) = (MonadReader r m, MonadIO m, MonadError String m)

runStack :: StackM r m => m ()
runStack = do
  liftIO $ putStrLn "good"
  throwError "Error"
  x <- ask
  return ()

------------------------------------------------------ ------------------------
-- type family implement printf
