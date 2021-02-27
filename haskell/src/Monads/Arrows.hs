{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module Monads.Arrows where

{-@ Arrow, new abstracation!
    Arrow represents compositin and data flow.

   --|f|-->
@-}

import           Data.Coerce        (coerce)
import           Data.Type.Coercion
import           Data.Type.Equality
import qualified GHC.Base           (id, (.))
import           GHC.Generics       (Generic, Generic1)
import           Prelude            hiding (id, (.))

infixr 9 .
infixr 1 >>>, <<<
infixr 3 ***
infixr 3 &&&

-- a category is like a category in cat theory.
-- category has identity morphism and morphism composition.
class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

-- rewrite rules, helps you to optimize your program.
-- rule name + rule form
{-# RULES
"identity/left"  forall p. id . p = p
"identity/right" forall p. p . id = p
"association"    forall p q r. (p . q) . r = p . q . r
#-}

instance Category (->) where
  id = GHC.Base.id
  (.) = (GHC.Base..)

instance Category Coercion where
  id = Coercion
  (.) Coercion = coerce


-- | right to left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f


{-@ Arrow typeclass @-}
class Category a => Arrow a where
  {-# MINIMAL arr, (first | (***)) #-}
  arr :: (b -> c) -> a b c

  first :: a b c -> a (b, d) (c, d)
  first = (*** id)

  second :: a b c -> a (d, b) (d, c)
  second = (id ***)

  -- split
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  f *** g = first f >>> arr swap >>> first g >>> arr swap
    where swap ~(x, y) = (y, x)

  -- fallout
  (&&&) :: a b c -> a b c' -> a b (c,c')
  f &&& g = arr (\b -> (b,b)) >>> f *** g

-- some nice optimization
{-# RULES
"compose/arr"   forall f g . arr f . arr g = arr (f . g)
#-}


instance Arrow (->) where
  arr f = f
  (***) f g ~(x, y) = (f x, g y)

-- define the kleisli arrow for composition.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

deriving instance Generic (Kleisli m a b)
deriving instance Generic1 (Kleisli m a)
deriving instance Functor m => Functor (Kleisli m a)
