module BinaryTreeMonad where

import           Data.Maybe

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show, Ord)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert a Leaf = Node Leaf a Leaf
treeInsert a (Node l b r)
  | a > b = Node l b (treeInsert a r)
  | a < b = Node (treeInsert a l) b r
  | otherwise = Node l b r

preorderHead :: BinaryTree a -> Maybe a
preorderHead Leaf         = Nothing
preorderHead (Node _ a _) = Just a

preorderLeft :: BinaryTree a -> BinaryTree a
preorderLeft Leaf         = Leaf
preorderLeft (Node l _ _) = l

preorderRight :: BinaryTree a -> BinaryTree a
preorderRight Leaf         = Leaf
preorderRight (Node _ _ r) = r

preorderTraverse :: BinaryTree a -> [a]
preorderTraverse t = go t []
  where go tree acc =
          case tree of
            Node Leaf a Leaf -> a : acc
            Node l a Leaf    -> a : go l acc
            Node Leaf a r    -> a : go r acc
            Node l a r       -> a : go l acc ++ go r acc
            Leaf             -> acc

instance Ord a => Monoid (BinaryTree a) where
  mempty = Leaf

instance Ord a => Semigroup (BinaryTree a) where
  t <> Leaf = t
  Leaf <> t = t
  t <> t' =   -- because treeInsert is not associative, can only use foldl here
    let go :: Ord a => [a] -> BinaryTree a -> BinaryTree a
        go tl tree = foldl (flip treeInsert) tree tl
    in go (preorderTraverse t) t'

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) =
    Node (fmap f l) (f a) (fmap f r)

instance Foldable BinaryTree where
  foldMap _ Leaf = mempty
  foldMap f t =
    let h = preorderHead t
        tl = preorderLeft t
        tr = preorderRight t
     in fromJust (f <$> h) <> foldMap f tl <> foldMap f tr

--  TODO:  <12-11-19, Jimmy> --
--  I need either a monoid instance for Bin tree without Ord typelclass constraint
--  or Monad and Applicative without using Monoidal append.
--  Since Applicative and Monad have instances for higher kinded type,
--  The higher kinded type must be fully polymorphic, you cannot apply
--  typeclass constraint on them.

instance Applicative BinaryTree where
  -- TODO: mapping applicative, now it doesn't works as expected
  pure a = Node Leaf a Leaf
  Leaf <*> _                       = Leaf
  Node Leaf f Leaf <*> Node _ a' _ = Node Leaf (f a') Leaf
  Node lf af rf <*> Node l' a' r'  = Node (lf <*> l') (af a') (rf <*> r')


