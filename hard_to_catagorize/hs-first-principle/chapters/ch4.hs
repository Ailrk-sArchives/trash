module Ch4  where

len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

-- pattern matching.
isPalindrome :: String -> Bool
isPalindrome "" = True
isPalindrome s =
    if reverse s == s  -- if-eles-then is expression.
       then True
    else
        False

absolute n =
    if n < 0
        then negate n
    else n

-- type alias.
type Name = String
-- data decalaration.
-- type constructor and data constructor.
data Pet = Cat | Dog Name

zipwap :: (a, b) -> (c, d) -> ((b, d), (a, c))
zipwap t1 t2 = (,) ((,) (snd t1) (snd t2)) ((,) (fst t1) (fst t2))

theHead :: [a] -> a
theHead = \ (x:xs) -> x


first :: (a, b) -> a
first = \ (a, b) -> a

-- polymorphism
-- parametric polymorphism
identity :: a -> a
identity = \x -> x

-- constrained polymorphism
-- bounded by the set of types with insance of Eq typeclass
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y

