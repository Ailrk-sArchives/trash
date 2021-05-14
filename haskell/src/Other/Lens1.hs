{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Other.Lens1 where

import           Data.Functor.Const
import           Data.Functor.Identity


{-@ Lens provides access into the middle of a data structure.
    Access:
      1. read
      2. write
      3. modify

    Lens' s a
    Lens' DateTime Min

    lens composes.

    Lens' s1 s2 -> Lens' s2 s3 -> Lens' s1 s3
@-}

-- solve nested record.
data Person = P { _name   :: String
                , _addr   :: Address
                , _salary :: Int
                }

data Address = A { _road     :: String
                 , _city     :: String
                 , _postcode :: String
                 }

setPostCode' :: String -> Person -> Person
setPostCode' pc p = p { _addr = (_addr p) { _postcode = pc } }

data LenR s a = L { viewR :: s -> a
                  , setR  :: a -> s -> s
                  , mod   :: forall f. Functor f => (a -> f a) -> s -> f s
                  }

-- we want to set and get.
-- we have these two represnetation.
-- In fact they are isomorphic.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

set :: forall s a. Lens' s a -> (a -> s -> s)
set ln x = runIdentity . ln (Identity . const x)

over :: forall s a. Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

view :: forall s a. Lens' s a -> s -> a
view ln = getConst . ln Const


-- now we can produce lenses.
name :: Lens' Person String
name f (P n a s) = fmap (\n' -> P n' a s) (f n)

addr :: Lens' Person Address
addr f (P n a s) = fmap (\a' -> P n a' s) (f a)

salary :: Lens' Person Int
salary f (P n a s) = fmap (\s' -> P n a s') (f s)

road :: Lens' Address String
road f (A r c p) = fmap (\r' -> A r' c p) (f r)

city :: Lens' Address String
city f (A r c p) = fmap (\c' -> A r c' p) (f c)

postcode :: Lens' Address String
postcode f (A r c p) = fmap (\p' -> A r c p') (f p)
