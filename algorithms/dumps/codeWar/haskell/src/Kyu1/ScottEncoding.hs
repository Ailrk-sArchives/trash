{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kyu1.ScottEncoding where

import           Prelude hiding (concat, curry, foldl, foldr, fst, length, map,
                          null, snd, take, uncurry, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair (SPair f) = f (,)

fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)

fst :: SPair a b -> a
fst (SPair f) = f const

snd :: SPair a b -> b
snd (SPair f) = f (const id)

swap :: SPair a b -> SPair b a
swap (SPair f) = SPair (f . flip)

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \a b -> f (SPair (\f' -> f' a b))

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \(SPair f') -> f' f

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe f) = f Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe = \case
  Just a  -> (SMaybe (\_ g -> g a))
  Nothing -> (SMaybe const)

isJust :: SMaybe a -> Bool
isJust (SMaybe f) = f False (const True)

isNothing :: SMaybe a -> Bool
isNothing = not . isJust

catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList f) = f nil' (\(SMaybe f') l ->
  let rest = (catMaybes l)
   in f' rest (flip cons rest))

toEither :: SEither a b -> Either a b
toEither (SEither f) = f Left Right

fromEither :: Either a b -> SEither a b
fromEither = \case
  Right r -> (SEither (\_ b -> b r))
  Left l  -> (SEither (\a _ -> a l))

isLeft :: SEither a b -> Bool
isLeft (SEither f) = f (const True) (const False)

isRight :: SEither a b -> Bool
isRight = not . isLeft

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition (SList f) = f (SPair (\f' -> f' nil' nil'))
  (\(SEither e) l' ->
    let SPair p = partition l'
     in p (\la lb ->
       e (\a -> (SPair (\f' -> f' (cons a la) lb)))
         (\b -> (SPair (\f' -> f' la (cons b lb))))))

toList :: SList a -> [a]
toList (SList f) = f [] (\a l -> a : toList l)

fromList :: [a] -> SList a
fromList = \case
  []     -> nil'
  (x:xs) -> cons x (fromList xs)

nil' :: SList a
nil' = (SList (\b _ -> b))

cons :: a -> SList a -> SList a
cons a b = SList (\_ f -> f a b)

concat :: SList a -> SList a -> SList a
concat (SList f) l = f l (\a l' ->
  cons a (concat l' l))

null :: SList a -> Bool
null (SList f) = f True (const . const False)

length :: SList a -> Int
length (SList f) = f 0 (\_ l -> 1 + (length l))

map :: (a -> b) -> SList a -> SList b
map f (SList la)= la nil' (\a l -> cons (f a) (map f l))

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList la) (SList lb) = la nil' (\a l ->
  lb nil' (\b l' ->
    cons (SPair (\f -> f a b)) (zip l l')))

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b l = foldr (\b g a -> g (f a b)) id l b

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b  (SList l) = l b (\a l' -> f a (foldr f b l'))

take :: Int -> SList a -> SList a
take n (SList l) = l nil'
  (\a l' ->
    case n of
      0 -> nil'
      _ -> cons a (take (n - 1) l'))
