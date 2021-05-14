{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Types.TypeableAndDatKinds where


-- Data and Typeable
-- encode type information that can be quried at runtime.

import           Data.Data
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable (Typeable, cast, typeOf)

strLen :: String -> Int
strLen = length

-- cast try to convert m to the designated type.
-- we convert a polymorphic type into a concrete type.

fn :: Typeable a => a -> Int  -- given any type a, if it's String, use it.
fn m =
  case (cast m :: Maybe String) of
    Just mi -> strLen mi
    Nothing -> error "Not a string"

runStrLen :: IO ()
runStrLen = putStrLn $ show $ fn "Is this a string?"


{-@ show case some DataKinds Techniques
    data kinds allows you to define a data type, and promote it to the data level.
    namely you have the type becomes kind and values becomes type.
@-}
-- promote type level enum. Now we defined 4 different types and a new kinds.
data Currency = 'INR | 'USD | 'AUD | 'YEN deriving (Show, Data, Typeable)

-- the kind of c is Curreny, so Money can only have four possible variations.
-- note Currency is a phanthom type, under the hood there is only one representation.
data Money :: Currency -> * where
  Money :: Typeable c => Int -> Money c

deriving instance Show (Money c)

usdMoney n = Money n :: Money USD
yenMoney n = Money n :: Money YEN
audMoney n = Money n :: Money AUD
inrMoney n = Money n :: Money INR

-- just make a baby step each time.
fromUSD :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromUSD m = do
  Money n <- cast m :: Maybe (Money USD)
  return n

fromYEN :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromYEN m = do
  Money n <- cast m :: Maybe (Money YEN)
  return n

fromAUD :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromAUD m = do
  Money n <- cast m :: Maybe (Money AUD)
  return n

fromINR :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromINR m = do
  Money n <- cast m :: Maybe (Money INR)
  return n

-- hide different money behind exitential type and pick the right type with typeable.
data M = forall (a :: Currency). Typeable a => M (Money a)

pickYen :: [M] -> [Money YEN]
pickYen [] = []
pickYen ((M x):xs) = maybe (pickYen xs) (\n -> n:pickYen xs) (cast x :: Maybe (Money YEN))

moneys = [ M $usdMoney 12
         , M $ yenMoney 39
         , M $ audMoney 23
         , M $ yenMoney 102
         , M $ usdMoney 39
         , M $ inrMoney 34
         ]

-- Type operators.
type family CurrencyOf a  where
  CurrencyOf (Money 'USD) = 'USD
  CurrencyOf (Money 'YEN) = 'YEN
  CurrencyOf (Money 'INR) = 'INR
  CurrencyOf (Money 'AUD) = 'AUD


-- instance Show (Money currency) where
--   show (Money a) = (show a) ++ (show $ typeOf (Proxy :: Proxy currency))

{-@ bring back types hidden behind exitential types.
@-}
