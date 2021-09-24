{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
module Types.KnownNatVec where

import           Data.Maybe
import           Data.Proxy
import qualified Data.Vector  as V
import           GHC.TypeLits

data Vec (n :: Nat) a = UnsafeMkVec { unVec :: V.Vector a }
  deriving Show

-- use natVal to reflect type level KnownNat to term level
termlevel10 = natVal (Proxy @10)

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v
  | V.length v == l = Just $ UnsafeMkVec v
  | otherwise = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

-- this function must be annotated because the value depends on type.
vec1 :: Vec 3 Int
vec1 = fromJust . mkVec $ V.fromList [1, 2, 3]


mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f v = UnsafeMkVec $ V.map f (unVec v)

instance Functor (Vec n) where
  fmap = mapVec

appendVec :: Vec n a -> Vec m a -> Vec (n + m) a
appendVec (UnsafeMkVec u) (UnsafeMkVec v) = UnsafeMkVec (u V.++ v)

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec (UnsafeMkVec u) (UnsafeMkVec v) = UnsafeMkVec (V.zip u v)

takeVec :: forall n m a . KnownNat n => Vec (n + m) a -> Vec n a
takeVec (UnsafeMkVec v) = let l = (fromIntegral . natVal $ Proxy @n)
                           in UnsafeMkVec (V.take l v)

splitVec :: forall n m a. KnownNat n
         => Vec (n + m) a
         -> (Vec n a, Vec m a)
splitVec (UnsafeMkVec v) = (UnsafeMkVec x, UnsafeMkVec y)
  where
    l = fromIntegral (natVal (Proxy @n))
    (x, y) = V.splitAt l v

index :: forall n m a . (KnownNat m, m <= n) => Vec n a -> Proxy m -> a
index (UnsafeMkVec v) m = v V.! l
  where l = (fromIntegral . natVal $ m)

vec1_1 = index vec1 (Proxy @0)
-- vec1_2 = index vec1 (Proxy @4)

-- create a vector with n length at runtime
runtimeMkVec :: Num a => Integer -> Vec n a
runtimeMkVec n = case someNatVal n of
                   Just (SomeNat (_ :: Proxy n)) -> UnsafeMkVec (V.fromList (replicate (fromIntegral n) 0))
                   _ -> error "I can't"

-- you can't append this two
vec2 = runtimeMkVec 10
vec3 = runtimeMkVec 5

vec4 = appendVec vec1 vec1
vec5 = appendVec vec1 vec2
vec6 = appendVec vec2 vec2

-- this only works for KownNats.
vecLength :: forall n a . KnownNat n => Vec n a -> Integer
vecLength _ = natVal (Proxy @n)

l1 = vecLength vec1
--- l2 = vecLength vec2

intToNatProxy :: Integer -> Proxy Nat
intToNatProxy n = case someNatVal n of
                    Just (SomeNat (_ :: Proxy n)) -> Proxy
                    _                             -> error "I can't"

-------------------------------------------------------------------------------
-- exitential type to hide gadt
-- there is no subtyping in haskell, only type equivalence and constraints.
-- So LNat 0 cannot be use in LNat r, because we can't prove 0 ~ r even though
-- it is the case.
--
-- To have function works with both LNat r and LNat 0 we need to hide their
-- type.
data LNat (r :: Nat) where
  LZ :: LNat 0
  LS :: LNat n -> LNat (n + 1)
deriving instance Show (LNat r)

-- the exitential wrapper.
data SomeLNat where
  SomeLNat ::  LNat r -> SomeLNat
deriving instance Show SomeLNat

-- the type is erased but the structure is preserved at term level. we can
-- simply collect constructors into a integer value.
lnatToInteger :: LNat r -> Integer
lnatToInteger LZ     = 0
lnatToInteger (LS m) = lnatToInteger m + 1

-- some nat 2 integer.
someLNat2Integer :: SomeLNat -> Integer
someLNat2Integer (SomeLNat l) = lnatToInteger l

instance Num SomeLNat where
  fromInteger 0 = SomeLNat $ LZ   -- wrap.
  fromInteger i = case fromInteger (i - 1) of
                    SomeLNat n -> SomeLNat (LS n)

  m + n = fromInteger $ someLNat2Integer m + someLNat2Integer n

type family FromLNat n :: Nat where FromLNat (LNat n) = n
