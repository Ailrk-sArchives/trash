{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Other.IndexedMonad where

-- allow us to enforce pre and post condition of a monad. It's helpful to
-- enforce protocols and contracts at the type level.

import           Control.Monad.Indexed
import           Data.Coerce

import           GHC.TypeLits          (Nat)
import qualified GHC.TypeLits          as TL
import           System.IO             hiding (Handle, openFile)
import qualified System.IO             as SIO

-- we can use indexed monad to force linearlity: a resource only can be used
-- once

newtype Ix m i j a = Ix { unsafeRunIx :: m a }
  deriving (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
  imap = fmap

instance Applicative m => IxPointed (Ix m) where
  ireturn = pure

-- need to coerce to use phantom type parameters i, j
instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b . Ix m i j (a -> b) -> Ix m j k a -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
  ibind :: forall i j k a b . (a -> Ix m j k b) -> Ix m i j a -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b


-- ensure monadic action in right order

-- use as type level index
data LinearState = LinearState
  { linearNextKey  :: Nat
  , linearOpenKeys :: [Nat]
  }

-- s to pin the local thread.
newtype Linear s (i :: LinearState) (j :: LinearState) a = Linear
  { unsafeRunLinear :: Ix IO i j a }
  deriving (IxFunctor, IxApplicative, IxMonad, IxPointed)

newtype Handle s key = Handle { unsafeGetHandle :: SIO.Handle }

-- Linear is 3 layers above (Handle), but they have exactly the same runtime
-- representations.
openFile :: FilePath -> IOMode
         -> Linear s ('LinearState next open)
                     ('LinearState (next TL.+ 1) (next ': open))
                     (Handle s next)
openFile = coerce SIO.openFile

