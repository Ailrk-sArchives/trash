{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Types.Ex where

import           Control.Monad.ST
import           Data.STRef
import           GHC.Generics

-- extential types --
-- for all types s that's in Show, there exists SB s.t SB s :: *
data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
  show (SB s) = show s

heterList :: [ShowBox]
heterList = [SB 1, SB True, SB "good"]

printShowboxes :: [ShowBox] -> IO ()
printShowboxes = mapM_ print

-- this is an any type
-- it just takes all types.
-- for all types a, exists All a
data AllBox = forall a. All a

-- forall can be thought of find the commonality of different types.
-- Note bottom is a member of all types!
-- So we are saying for the commonality of all types, we have All a
-- => The only commanlity of all types is they have bottom time as a member.
-- => All types are part of All a
anylist = [All 1, All "asd", All heterList]

-- why we need a box anyway?
-- see this
-- [forall a. a]  a list of forall a, the common types of all a, so... [_|_]
-- [forall a. Show a => a]  list of forall a that is part of Show, the common type of them. So [_|_]
-- [forall a. Num a => a] list of all a that's part of Num a, the common type of them. So [_|_]

-- but This is different:
data T = forall a. MkT a
-- type of MkT :: forall a. a -> T
-- a list allows any types, but we can't do anythingto it because types
-- are forall a. a
tlist = [MkT (), MkT "asd", MkT 12]

data G = forall a. (Num a, Show a) => MkG a
-- type of MkG :: a. (Num a, Show a) => MkG a
-- Now we can do something based on the type constraint.
glist = [MkG 1, MkG 1.2]


-- Type class provides you the ability to have sub class,
-- but sub class are relationships between the type of types,
-- instead of individual type.
-- How do you inherit from a particular implementation though?
class (Show a) => Base a where
  base :: a -> String
  base a = "Base " ++ show a

class Base a => Derived a where
  derived :: a -> String

data B = forall a. Base a => B a
data D = forall a. Derived a => D a

data B1 = B1 deriving Show

newtype B1' = B1' B1


instance Base B1 where
  base B1 = "Base " ++ show B1

data D1 = D1 deriving (Show, Base)



-- -- can't inherit directly
-- instance Base D1 where
--   base D1 =  "Base " ++ show D1

-- instance Derived D1 where
--   derived D1 = "Derived " ++ show D1

{-@ The biggest application: RankNType with runST @-}

-- runST :: (forall s. ST s a) -> a
-- the state cannot escape the ST action, so runST constraint the
-- effect within the scope.
localMut :: Int -> Int
localMut n = runST $ do
  ref <- newSTRef n
  modifySTRef' ref (+1)
  readSTRef ref

-- define pair with universal quantification
newtype Pair a b = Pair (forall c. (a -> b -> c) -> c)

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \f -> f a b
