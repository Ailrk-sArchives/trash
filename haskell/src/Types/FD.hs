{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}

{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.FD where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

class Coll c e | c -> e where
  empty :: c
  insert :: c -> e -> c
  member :: c -> e -> Bool


-- [a] determines e should be a.
instance Eq a => Coll [a] a where
  empty = []
  insert xs a = a : xs
  member xs e = e `elem` xs

-- type checker infer to c -> e -> e -> c instead of c -> e1 -> e2 -> c
ins2 xs a b = insert (insert xs a) b

-- without functional dependencies even with same c in the context we can't say
-- what type e will be, so you can only assume different instances of e are different types.
class Coll' c e where
  empty' :: c
  insert' :: c -> e -> c
  member' :: c -> e -> Bool


{-@ Undecidable instance ?

    - It's commonly needed for multiparamter typeclass when recurse on the non functional
    dependend part.

    - Haskell wants all instance to be decidable by default, but in lots' of cases it's just
    not possible.

@-}

-- Bad example:

-- class MonadIO' (m :: * -> *) where
--   liftIO' :: IO a -> m a

-- this is an example of a generic instance for all MonadIO of type (t :: (* -> *) -> * -> *)
-- and m :: * -> *.
-- It works for all instances with (t m) being a monad, t being a monad trans and m being a
-- MonadIO.


-- PROBLEM of this instance --
--    1. overlapping
--        It's too generic and exlucde people implementing instance with the same shape.
--    2. undecidable instance
--        It's undecidable because constraint (Monad (t m)) has the same type parameter
--        as (MonadIO (t m)), no constructor is removed. Haskell requires constraint
--        of instance at least remove one type constructor to make sure it's smaller
--        than the instance head.
instance (Monad m, Monad (t m), MonadIO m, MonadTrans t) => MonadIO (t m) where
  liftIO = lift . liftIO

data SomeEnv = SomeEnv
  { v1 :: Int
  , v2 :: String
  , v3 :: forall a. [(String, a)]
  }

data Rec = Rec
  { intList    :: [(String, Int)]
  , stringList :: [(String, String)]
  }

-- now let's make an arbitray transformer stack.
newtype MTStack m a = MTStack { unMTStack :: ReaderT SomeEnv (StateT Rec m) a }
  deriving (Functor, Applicative, Monad, MonadReader SomeEnv, MonadState Rec)


instance MonadTrans MTStack where     -- still need to make MTStack a monad transformer.
  lift ma = MTStack $ do
    a <- lift . lift  $ ma
    return a

-- This will be an overlapping instance! Because the generic isntance defined above already
-- can be applied to MTStack m a.
-- instance MonadIO (MTStack IO) where
--   liftIO = lift . liftIO

foo :: MTStack IO ()
foo = do
   v <- v1 <$> ask
   liftIO $ putStrLn "asd"
   return ()


{-@ Why UndecidableInstances is usually needed for MPTC and FD?
@-}

-- first try define a mtl style MonadState.
-- It's a MPTC with function dependency, it says given monad m, the state of
-- MonadState is uniquely defined to be s.
class Monad m => MonadState' s m | m -> s where
  get' :: m s
  get' = state' (\s -> (s, s))

  put' :: s -> m ()
  put' s = state' (\_ -> ((), s))

  state' :: (s -> (a, s)) -> m a
  state' f = do
    s <- get'
    let (a, s') = f s
    put' s
    return a

-- Next we make an instance of MonadState for ReaderT

-- According to haskell's standard, this is undecidable instance
-- A decidable instance needs to have a smaller constraint than then instace head.
-- All constraints should at least eliminate one type constructor, so by induction haskell
-- knows the instances for sure terminates.
--
-- Because MonadState is MPTC, our instance only eliminates constructor of (ReaderT e m)
-- but not on s, so it's undecidable.
--
-- But it's really decidable because s is determined by m, so if m is shrinking it doesn't
-- matter what s is since it's a function of m.
--
-- So in these case it's ok to use undecidable instances.
--
instance MonadState' s m => MonadState' s (ReaderT e m)
-- this instance uses default implementation:


{-@ UndecidableInstance is also ok when you're using typeclass as alias of several
    Smaller constraints.
@-}

class Foo a
class Bar a
class (Foo a, Bar a) => FooBar a


{-@ We can replace FD with type family. This way we don't need to show the
    function dependency in the instance context, thus no undecidable instances is
    needed.
@-}

class Monad m => MonadState'' m where
  type MState m :: *
  get'' :: m (MState m)
  get'' = state'' (\s -> (s, s))

  put'' :: (MState m) -> m ()
  put'' s = state'' (\_ -> ((), s))

  state'' :: (MState m -> (a, MState m)) -> m a
  state'' f = do
    s <- get''
    let (a, s') = f s
    put'' s'
    return a

-- don't need functional dependency any more, and s is not in context, so no undecidable
-- instances.
instance MonadState'' m => MonadState'' (ReaderT e m) where
  type MState (ReaderT e m) = e

{-@ Conclusion:
    - You still don't want undecided instances.
    - Cases that undecided instances are ok is because they are inheriently decidable.
      For instance MPTC with functional dependencies is really decidable if you only recurse
      on the non depended parameter.
    - You may say the need of undecided instances for MPTC and FD is a design flaw.

    - side1: When writing generic instance you should be extra careful not to make it to generic
             and pollute other potential instances.

    - side2: Transformer with mtl doesn't make the stack itself transformer, you still need to
             implement the transformer instance. Specifically, how many levels you need to tell
             the instance lift the bottom monad up to perform it's effects.

    - side3: Monad transformer really just implement an interface on top of the stacked newtype,
             the reason we can avoid lift is because all actions are lifted into the same level
             already.

    - side4: For state monad the state function is really the core. All state transformation is just
             take the old state and return new state and retur value.
             The concept is not only for state monad, but anything need functional update.
@-}
