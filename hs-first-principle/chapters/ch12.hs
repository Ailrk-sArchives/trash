module Ch12 where

-- string processing

notThe :: String -> Maybe String
notThe s =
    if s == "The" || s == "the"
       then Nothing
    else Just s

tokenize :: String -> [String]
tokenize str = go str ""
    where
        go "" acc = (reverse acc) : []
        go (a:as) acc
          | a == ' ' = (reverse acc) : (go as "")
          | otherwise = go as (a : acc)

hasVowel :: String -> Bool
hasVowel "" = False
hasVowel (x:xs) =
    let vowels = "aeiou"
     in (elem x vowels) || hasVowel xs


replaceThe :: String -> String
replaceThe s =
    let tokens = tokenize s
     in concat (foldr (\x y -> case notThe x of
                         Nothing -> "a " : y
                         _ -> x : " ": y)
              [] tokens)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s =
    let tokens = tokenize s
     in foldr (\x y -> case () of
                         _ | notThe x == Nothing -> 1 + y
                           | hasVowel x -> 0
                           | otherwise -> y)
        0 tokens

countVowels :: String -> Integer
countVowels "" = 0
countVowels (x:xs) =
    if elem x "aeiou"
       then 1 + countVowels xs
    else countVowels xs

countConsonants :: String -> Integer
countConsonants s = (fromIntegral . length $ s) - (countVowels s)

newtype Word' =
    Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w
  | countVowels w > countConsonants w = Nothing
  | otherwise = Just $ Word' w

-- Natural number
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ xs) = (+1) . natToInteger $ xs

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just (go n)
        where
            go n
                | n == 0 = Zero
                | otherwise = Succ (go (n - 1))

-- lib for Maybe, some catamorphism here.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing x = not (isJust x)

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f (Just m) = (f m)
mayybee z f Nothing = z

fromMaybe :: a -> Maybe a -> a
fromMaybe z (Just x) = x
fromMaybe z Nothing = z

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
                     Nothing -> catMaybes xs
                     (Just a) -> a : catMaybes xs

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe t@(x:xs)
  | any (==Nothing) t = Nothing
  | otherwise = Just (go (x:xs))
        where
            go [] = []
            go (Just a:as) = a : go as

-- lib for Either
lefts :: [Either a b] -> [a]
lefts [] = []
lefts (x:xs) = case x of
                 (Left a) -> a : lefts xs
                 _ -> lefts xs

rights :: [Either a b] -> [b]
rights xs = foldr (\x y -> case x of
                             (Right a) -> a : y
                             _ -> y)
            [] xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = go xs ([], [])
    where
        go [] acc = acc
        go (a:as) (l1, l2) =
            case a of
              (Left a) -> go as (a:l1, l2)
              (Right a) -> go as (l1, a:l2)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe f (Right e) = Just $ f e
eitherMaybe f (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToc _ (Left a) = aToc a
either' _ bToc (Right b) = bToc b

-- write your own iterate and unfoldr, anamorphism.
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z =
    case (f z) of
      Just (a, b) -> a : myUnfoldr f b
      Nothing -> []

-- binary tree practices.
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f x =
    case (f x) of  -- unpack the return value!
      Nothing -> Leaf
      Just (a, b, c) -> Node (unfoldTree f a) b (unfoldTree f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n =
    unfoldTree (\x -> if x >= n
                         then Nothing  -- very expressive here.
                      else Just (x+1, x, x+1)) 0




















