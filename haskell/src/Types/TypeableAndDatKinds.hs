{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Types.TypeableAndDatKinds where


-- Data and Typeable
-- encode type information that can be quried at runtime.

import           Data.Data
import           Data.Dynamic
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

usdMoney n = Money n :: Money 'USD
yenMoney n = Money n :: Money 'YEN
audMoney n = Money n :: Money 'AUD
inrMoney n = Money n :: Money 'INR

-- just make a baby step each time.
fromUSD :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromUSD m = do
  Money n <- cast m :: Maybe (Money 'USD)
  return n

fromYEN :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromYEN m = do
  Money n <- cast m :: Maybe (Money 'YEN)
  return n

fromAUD :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromAUD m = do
  Money n <- cast m :: Maybe (Money 'AUD)
  return n

fromINR :: forall (a :: Currency). Typeable a => Money a -> Maybe Int
fromINR m = do
  Money n <- cast m :: Maybe (Money 'INR)
  return n

-- hide different money behind exitential type and pick the right type with typeable.
data M = forall (a :: Currency). Typeable a => M (Money a)

pickYen :: [M] -> [Money 'YEN]
pickYen [] = []
pickYen ((M x):xs) = maybe (pickYen xs) (\n -> n:pickYen xs) (cast x :: Maybe (Money 'YEN))

moneys = [ M $usdMoney 12
         , M $ yenMoney 39
         , M $ audMoney 23
         , M $ yenMoney 102
         , M $ usdMoney 39
         , M $ inrMoney 34
         ]

-- Type operators.
type family CurrencyOf a :: Currency where
  CurrencyOf (Money 'USD) = 'USD
  CurrencyOf (Money 'YEN) = 'YEN
  CurrencyOf (Money 'INR) = 'INR
  CurrencyOf (Money 'AUD) = 'AUD

-- instance Show (Money currency) where
--   show (Money a) = (show a) ++ (show $ typeOf (Proxy :: Proxy currency))

{-@ bring back types hidden behind exitential types.
@-}

data MoneyEx' = forall x. MoneyEx' (Money x)


-- when adding two moneys, we want them to be of the same currency, thus c to be the same
-- type.
moneyAdd :: Money c -> Money c -> Money c
moneyAdd (Money a) (Money b) = (Money (a + b))

-- this doesn't work because we erased the type. After we bring the value back all we know
-- about the type is it's money. We don't know, for example, whether two types are the same.
-- moneyAddEx' :: MoneyEx' -> MoneyEx' -> MoneyEx'
-- moneyAddEx' (MoneyEx' a) (MoneyEx' b) = MoneyEx' $ moneyAdd a b

-- we can solve this with typeable.

data MoneyEx = forall x. (Typeable x) => MoneyEx (Money x)

deriving instance Show MoneyEx

-- this will throw an error when a and b are not of the same type.
moneyAddEx :: MoneyEx -> MoneyEx -> MoneyEx
moneyAddEx (MoneyEx a) (MoneyEx b) = case cast a of
                                       Just a1 -> MoneyEx $ moneyAdd a1 b
                                       Nothing -> error "not the same type"

-- this works.
addusd1 = moneyAddEx (MoneyEx (Money 12 :: Money 'USD)) (MoneyEx (Money 10 :: Money 'USD))

-- this throws a runtime error.
addusd2 = moneyAddEx (MoneyEx (Money 12 :: Money 'USD)) (MoneyEx (Money 10 :: Money 'AUD))


{-@ Use dynamics to replce exitential types
    Dynamic types provide operations to convert dynamic values to a monomorphic type.
    Still use Typeable under the hood.
@-}
-- using exitential type means we have to erase the type info, and later if we want to restore
-- them we somehow need to test the type at runtime. (Typeable.)
-- Another approach is to use Data.Dynamic

data IntWrapper = IntWrapper Int deriving (Typeable)

-- wrap into dynamic varaible
d0 = toDyn (IntWrapper 1)
d1 = toDyn (MoneyEx (Money 12 :: Money 'USD))

d2 = fromDyn d1 (Money 0 :: Money 'USD)
d3 = fromDyn d0 (Money 0 :: Money 'USD)


{-@ Conclusion
    1. Typeable is what gives you runtime type information check.

    2. Using the function cast, you can check whether a type is the one you want.

    3. case on type is doing pattern matching on type varaible, this violate parametricity.

    4. Having different behaviors based on different type based on the result of cast is really
       another type of adhoc polymorphism.

    5. Datakinds promotes types and values to kinds and types.

    6. At type level kinds indicates a set of types that's acceptable.

    7. Type family is the bread and butter of the type level computation. It's the function at
       type level.

    8. Type family also pattern matches on type variable. As typeable, it also break the
       parametricity

    9. Typeable can be used to restore types hiden behind exitential type constructors.


    10. Or better, we can use Data.Dynamic to smuggle some types without GHC notices.
@-}
