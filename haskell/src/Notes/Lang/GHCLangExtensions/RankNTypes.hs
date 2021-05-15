{-# LANGUAGE RankNTypes #-}
module Notes.Lang.GHCLangExtensions.RankNTypes where


length' :: forall a. [a] -> Int
length' =  foldr (\a b -> b + 1) 0

fst' :: forall a b. (a, b) -> a
fst' (a, b) = a

snd' :: forall a b. (a, b) -> b
snd' (a, b) = b

map' :: forall a b. (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs
