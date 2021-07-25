{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeOperators              #-}
module Types.DataTypeGenericProgramming where


{-@ Map datatypes to equivalent representation
    do some operations on the representation
    map back.

    e.g map a sum type to A :+: B, which can be representated as one single
        type. Then pattern match on each branch.

    advantage: works on anyshape, because the mapping is automatic.
@-}

-- the SYB (scraping your boilerplates) style datatype generic programming
import           Data.Data     as D
import           Data.Typeable as T

-- the new Generic programming library with less overhead.
import           GHC.Generics  as G

-- first we can define some combinators to represent datatypes.
data I r = I r
data K a r = K a
data U r = U    -- unit. constructor with no arguments
data (f :+: g) r = L (f r) | R (g r)
data (f :*: g) r = f r :*: g r
data Fix (f :: * -> *) = In (f (Fix f))

instance Functor (K a) where
  fmap _ (K a) = K a

instance Functor I where
  fmap f (I a) = I (f a)
