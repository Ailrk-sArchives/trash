{-@ How do you write mtl like interface?
    (Although it's called mtl like interface, MonadFix, MonadFail and
     MonadIO are all in the prelude. The library is so inconsistent and
     takes you extra effort to understand a simple thing...)
    Here is some little practise
@-}

-- First of all, if you want mtl you need multiparameter type classes
-- and functional dependencies.
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- Of course the entire mtl is based on newtype deriving
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Monads.MyMTL where
import           Data.Functor.Classes  (Eq1 (..), Ord1 (..), Read1 (..),
                                        Show1 (..), compare1, eq1, readsData,
                                        readsUnaryWith, showsUnaryWith)
import           Data.Functor.Identity


-- Identity --
-- This is probably the best example to play around with mtl facilities

newtype IdentityT f a = IdentityT { runIdentity :: f a }

instance (Eq1 f) => Eq1 (IdentityT f) where
  liftEq eq (IdentityT x) (IdentityT y) = liftEq eq x y

instance (Ord1 f) => Ord1 (IdentityT f) where
  liftCompare comp (IdentityT x) (IdentityT y) = liftCompare comp x y

instance (Read1 f) => Read1 (IdentityT f) where
  liftReadsPrec rp r1 = readsData $
    readsUnaryWith (liftReadsPrec rp r1) "IdentityT" IdentityT

instance (Show1 f) => Show1 (IdentityT f) where
  liftShowsPrec sp s1 d (IdentityT m) =
    showsUnaryWith (liftShowsPrec sp s1) "IdentityT" d m

-- use Eq1, Ord1... etc to implement Eq, Ord... directly
instance (Eq1 f, Eq a) => Eq (IdentityT f a) where
  (==) = eq1
instance (Ord1 f, Ord a) => Ord (IdentityT f a) where
  compare = compare1


-- StateT --
type State s = StateT s Identity

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
