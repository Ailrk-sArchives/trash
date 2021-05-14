{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Types.TypeFamZipper where

import           Control.Exception
import           Control.Monad.Except
import           Data.Maybe

data TypeList l where
  Nil :: TypeList '[]
  (:::) :: a -> TypeList l -> TypeList (a ': l)

infixr :::

type family Head ls where Head (TypeList (x ': _)) = x
type family Tail ls where Tail (TypeList (_ ': xs)) = xs

type family Foldr (f :: * -> * -> *) b ls where


-- type synonyms families

class Zippers c where
  type Focus c :: *
  type Context c :: *
  moveLeft :: c -> c
  moveRight :: c -> c
  modify :: (Focus c -> Focus c) -> c -> c
  focus :: c -> Maybe (Focus c)

data ListZipper a = ListZipper [a] a [a]
                  | EmptyListZipper

deriving instance Show a => Show (ListZipper a)

instance Zippers (ListZipper a) where
  type Focus (ListZipper a) = a
  type Context (ListZipper a) = ([a], [a])

  moveLeft (ListZipper (l : ls) x rs) = ListZipper ls l (x : rs)
  moveLeft EmptyListZipper            = EmptyListZipper

  moveRight (ListZipper ls x (r : rs)) = ListZipper (x : ls) r rs
  moveRight EmptyListZipper            = EmptyListZipper

  modify f (ListZipper ls x rs) = ListZipper ls (f x) rs
  modify _ EmptyListZipper      = EmptyListZipper

  focus (ListZipper _ c _) = Just c
  focus EmptyListZipper    = Nothing


data Tree a = Branch a (Tree a) (Tree a) | Leaf
data TreeDir a = TreeLeft a (Tree a) | TreeRight a (Tree a)
type TreeDirs a = [TreeDir a]
data TreeZipper a = TreeZipper (Tree a) (TreeDirs a)
                  | EmptyTreeZipper

deriving instance Show a => Show (Tree a)
deriving instance Show a => Show (TreeDir a)
deriving instance Show a => Show (TreeZipper a)

instance Zippers (TreeZipper a) where
  type Focus (TreeZipper a) = a
  type Context (TreeZipper a) = TreeDirs a
  moveLeft (TreeZipper (Branch a lt rt) bs) = TreeZipper lt (TreeLeft a rt : bs)
  moveLeft EmptyTreeZipper                  = EmptyTreeZipper

  moveRight (TreeZipper (Branch a lt rt) bs) = TreeZipper rt (TreeRight a lt : bs)
  moveRight EmptyTreeZipper = EmptyTreeZipper

  modify f (TreeZipper (Branch a lt rt) bs) = TreeZipper (Branch (f a) lt rt) bs
  modify _ EmptyTreeZipper                  = EmptyTreeZipper

  focus (TreeZipper (Branch a _ _) _) = Just a
  focus (TreeZipper _ _)              = Nothing

-- equality constraint.
-- given two zipper c1, c2, we want they have the same Focus
move2ZippersLeft :: ( Zippers c1
                    , Zippers c2
                    , Focus c1 ~ Focus c2
                    , f1 ~ Focus c1
                    , f2 ~ Focus c2
                    ) => c1 -> c2 -> (f1, f2)
move2ZippersLeft c1 c2 = (fromJust $ focus (moveLeft c1), fromJust $ focus (moveLeft c2))


listz = ListZipper [1..10] 11 [12..20]
treez = TreeZipper (Branch 10
                     (Branch 20
                       (Branch 40 Leaf Leaf)
                       Leaf)
                     (Branch 30 Leaf Leaf))
                     []

newfocus = move2ZippersLeft listz treez

moveLeftN :: forall a. Zippers a => a -> Int -> a
moveLeftN z 0 = z
moveLeftN z n = moveLeftN (moveLeft z) (n - 1)

moveRightN :: forall a. Zippers a => a -> Int -> a
moveRightN z 0 = z
moveRightN z n = moveRightN (moveRight z) (n - 1)
