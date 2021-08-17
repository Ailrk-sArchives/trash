{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# LANGUAGE KindSignatures      #-}
module Types.Dynamics where

import           Control.Monad (foldM_)
import           Data.Foldable (asum, traverse_)
import           Data.Kind
import           Data.Maybe    (fromMaybe)
import           Data.Typeable

-- exitential types hide type info. But if we add typeclass constraints,
-- value will be stored along side it's dictionary that we can call at runtime

-------------------------------------------------------------------------------
-- To have some runtime behavior we more or less need some dictionary passing
-- type technqiues.
data Showable where
  Showable :: (Typeable a, Show a) => a -> Showable

showables = [Showable (1 :: Int), Showable "a", Showable 'n']
run1 = traverse_ (\(Showable a) -> putStrLn $ show a) showables

-------------------------------------------------------------------------------
-- Typeable class provides type info at runtime.

-- the type of a is only known at runtime.
runtimeIsInt :: Showable -> Bool
runtimeIsInt (Showable a) =
  case cast a :: (Maybe Int) of
    Just n  -> True
    Nothing -> False

-- >>> runtimeIsInt (Showable (1 :: Int))
-- >>> runtimeIsInt (Showable "string")
-- True
-- False

-------------------------------------------------------------------------------
-- Dynamic erase types but storing type info with it, so when we need it at
-- runtime we can retreive it.
-- We know we can retrive rtti because t needs to be a Typeable
data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic k (Dynamic a) = k a

test1 = elimDynamic f
  where
    f n = case (cast n) :: Maybe Int of
            Just n' -> n' + 1
            Nothing -> 0

-- >>> test1 (Dynamic (1 :: Int))
-- >>> test1 (Dynamic ("sd"))
-- 2
-- 0

----- Get value from typeable

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

-- to use fromDynamic you need to let it know what a it's expecting.
-- >>> fromDynamic @Int (Dynamic (1 :: Int))
-- >>> fromDynamic @String (Dynamic (1 :: Int))
-- Just 1
-- Nothing

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r)
       => Dynamic
       -> Dynamic
       -> (a -> b -> r)
       -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

-- >>> (a, b) = (Dynamic (1 :: Int), Dynamic (2 :: Int))
-- >>> let addddyn = liftD2 @_ @_ @Int a b (+)
-- >>> fmap (fromDynamic @Int) addddyn
-- Just (Just 3)

-------------------------------------------------------------------------------
-- Make a python like add operation
-- it's not quite the same becasue you still need to type ap the resulting
-- value. True dynamic langauges should have values always in Dynamic and
-- never come out.

pyAdd :: Dynamic -> Dynamic -> Dynamic
a `pyAdd` b = fromMaybe (error "bad type")
            $ asum [ liftD2 @String @String a b (++)
                   , liftD2 @Int a b (+)
                   , liftD2 @String @Int a b $ \s n -> s ++ show n
                   , liftD2 @Int @String a b $ (\n s -> show n ++ s)
                   ]

-- >>> (a, b, c) = (Dynamic (1 :: Int), Dynamic (2 :: Int), Dynamic "a")
-- >>> fromDynamic @Int (a `pyAdd` a)
-- >>> fromDynamic @String (a `pyAdd` c)
-- >>> fromDynamic @String (c `pyAdd` a)
-- >>> fromDynamic @String (c `pyAdd` c)
-- Just 2
-- Just "1a"
-- Just "a1"
-- Just "aa"

-------------------------------------------------------------------------------
-- Generalized constraint kineded exitentials.

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas k (Has n) = k n

-- refactor out.
type HasShow1 = Has Show
type HasDynamic1 = Has Typeable
