module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

-- traverse binary tree
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node (Node Leaf 2 Leaf) 5 Leaf)



preorder :: BinaryTree a -> [a]
preorder t = go t []
    where go tree acc =
            case tree of
              Node Leaf a Leaf -> a : acc
              Node left a Leaf -> a : go left acc
              Node Leaf a right -> a : go right acc
              Node left a right -> a : go left acc ++ go right acc
              Leaf -> acc

inorder :: BinaryTree a -> [a]
inorder t = go t []
    where go tree acc =
            case tree of
              Node Leaf a Leaf -> a : acc
              Node left a Leaf -> go left acc ++ [a]
              Node Leaf a right -> a : go right acc
              Node left a right -> go left acc ++ [a] ++ go right acc
              Leaf -> acc

postorder :: BinaryTree a -> [a]
postorder t = go t []
    where go tree acc =
            case tree of
              Node Leaf a Leaf -> a : acc
              Node left a Leaf -> go left acc ++ [a]
              Node Leaf a right -> go right acc ++ [a]
              Node left a right -> go left acc ++ go right acc ++ [a]
              Leaf -> acc

-- catamorphisms of tree structures.
-- lazy eval
foldTreer :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreer _ z Leaf = z
foldTreer f z (Node left a right) =
    f a (foldTreer f (foldTreer f z left) right)

-- strict tail call
foldTreel :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreel _ z Leaf = z
foldTreel f z (Node left a right) =
    foldTreel f (foldTreel f (f a z) left) right

-- strict tail call
foldTreei :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreei _ z Leaf = z
foldTreei f z (Node left a right) =
    foldTreei f (f a (foldTreei f z left)) right





