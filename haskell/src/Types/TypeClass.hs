{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- http://okmij.org/ftp/Computation/typeclass.html

module Types.TypeClass where

import           Data.Dynamic
import           Data.Kind
import           Data.List
import           Data.Typeable
import           GHC.TypeLits
import Data.Maybe

-- you don't need typeclass
data UUID

-- we create an newtype wrapper that can have an unique value for each
-- specific type a.
-- Each concrete record is an overload.

-------------------------------------------------------------------------------

type instance Eq' UUID = TypeError (Text "bad" :<>: ShowType UUID)


-- define class where
type family Eq' a :: Constraint
data Eq'Dict a = Eq' a => Eq'Dict { equal :: a -> a -> Bool }
-- end define class

-- define instance where
type instance Eq' Bool = ()
eqBool :: Eq'Dict Bool
eqBool = Eq'Dict { equal = eq }
  where
    eq True True   = True
    eq False False = True
    eq _ _         = False
-- end define instance

-- define instance where
type instance Eq' Int = ()
eqInt :: Eq'Dict Int
eqInt = Eq'Dict { equal = eq }
  where
    eq a b = a - b == 0
-- end define instance

-- we have additional constraints, pass them as implicit paramters.
-- define instance where
type instance Eq' (a, b) = (Eq' a, Eq' b)
eq2Tuple :: (Eq' a, Eq' b) => (Eq'Dict a, Eq'Dict b) -> Eq'Dict (a, b)
eq2Tuple ctxs@(d_a, d_b) = Eq'Dict { equal = eq }
  where
    eq (a1, b1) (a2, b2) = (equal d_a a1 a2) && (equal d_b b1 b2)
-- end define instance

all' :: Eq' a => Eq'Dict a -> [a] -> Bool
all' _ [] = True
all' _ [x] = True
all' d_ (x:y:xs) = f x y && all' d_ xs
  where
    f = equal d_

-------------------------------------------------------------------------------
-- Another example
-- actually we don't need a type family return constraint, because the existance
-- of a dict Num'Dict Int is already a proof that there is an instance for
-- Num' Int.
--
-- define class where
type family Num' a :: Constraint
data Num'Dict a = Num' a => Num'Dict { plus    :: a -> a -> a
                                     , fromInt :: Int -> a
                                     }
-- end define class

-- define instance where
type instance Num' Int = ()
numInt :: Num'Dict Int
numInt = Num'Dict { plus = (+)
                  , fromInt = id
                  }
-- end define instance

-- define instance where
type instance Num' Double = ()
numDouble :: Num'Dict Double
numDouble = Num'Dict { plus = (+)
                     , fromInt = fromIntegral
                     }
-- end define instance

lookup' :: Eq' a => Eq'Dict a -> a -> [(a, b)] -> Maybe b
lookup' _ _ [] = Nothing
lookup' ctx@(Eq'Dict equal) k ((a, b):xs)
  | equal k a = Just b
  | otherwise = lookup' ctx k xs

member' :: Eq' a => Eq'Dict a -> a -> [a] -> Bool
member' ctx k [] = False
member' ctx k (x:xs)
  | (equal ctx) k x = True
  | otherwise = member' ctx k xs

-- zip with
removeDups :: (Num' a, Eq' a) => (Num'Dict a, Eq'Dict a) -> [a] -> [a]
removeDups _ [] = []
removeDups (numdict, eqdict) xs = go xs []
  where
    go [] as = as
    go (x:xs) as = if member' eqdict x as
                      then go xs as
                      else go xs (x:as)

-------------------------------------------------------------------------------
--

-- define class where
-- more constraints are still passed as dictionaries.
type family Mul' a :: Constraint
data Mul'Dict a = (Eq' a, Num' a) => Mul'Dict
  { times :: (Eq'Dict a, Num'Dict a) -> a -> a -> a }
-- end define class

-- define instance where
type instance Mul' Int = ()
intMul = Mul'Dict { times = f }
  where
    f :: (Eq'Dict Int, Num'Dict Int) -> Int -> Int -> Int
    f _ 0 _ = 0
    f _ _ 0 = 0
    f _ 1 x = x
    f _ x 1 = x
    f ctx@(eq_d_, num_d_) a b
      | a > 0 = f ctx (a - 1) ((plus num_d_) b 1)
      | otherwise = negate (f ctx (negate a) b)
-- end define instance

-- NOTE: conclusion 1: For typeclass, it's type predicate property is maintained
--       before the double arrow as the type context, but the actual overload
--       (term level values) are replaced by extra dictionary.
--       So it's appropriate to think the type conext has nothing to do with
--       term level values, but typeclass give us a convinent bridge between the
--       two.


-------------------------------------------------------------------------------
-- There are other ways to compile typeclasses.

-------------------------------------------------------------------------------
-- 1. Type calss as macros (monomorphization)
--
-- This is essentially haskell's C++ templates. It simply rewrite typeclass
-- into equivalent simple code and insert into the source during compilation.
--
-- If we do this after type checking, we don't really need type constraints
-- at all. Since the code is already type checkd, all we need is to pass the
-- right instance to the right function.
-- Let's see how monomorphization transform code.
bool_to_string = show @Bool
int_to_string = show @Int

class Show'Mono a where
  show'mono :: a -> String

instance Show'Mono Int where
  show'mono = int_to_string

instance Show'Mono Bool where
  show'mono = bool_to_string

test_show'mono :: String
test_show'mono = show'mono True

print'mono :: forall a. Show'Mono a => a -> IO ()
print'mono = putStrLn . show'mono

print_ints :: IO ()
print_ints = do
  traverse print'mono [1 :: Int, 2, 3]
  return ()

-----------------------
-- this compiles to the following code with monomophization:
-- No typeclass, no instance, no overloaded functions. No bounded polymorphism.
-- In monomorphization, overloaded functions are not really functions, but
-- macros that generate the corresponding function.

test_show'monoCompiled :: String
test_show'monoCompiled = bool_to_string True

-- this becomes a template, and it will be instantiated at call site.
-- print'mono'Compiled :: a -> IO ()

-- note that the polymorphic function is inlined all together.
-- this is a complete time process.
print_intsCompiled :: IO ()
print_intsCompiled = do
  traverse (putStrLn . int_to_string) [1 :: Int, 2, 3]
  return ()

-- NOTE: monomorphization is a partial evaluation of the result of
-- dictionary-passing translation.

-- NOTE: monomorphization lacks of support for separate compilation becasue
-- instantiation needs to be recompiled for all call site if the original
-- function is changed.

-- NOTE: monomorphization doesn't support runtime polymorphism, so it's still
-- necessary to have dictionary passing

-- NOTE: GHC use both, first it does full program dictionary passing, then it
-- will do partial evaluation to monomorphize possible cases.
-- Thus the compiled program is a mixture of dictionary passing and
-- monomorphization.

-- NOTE: monomorphization makes IO and State monad efficient.


-------------------------------------------------------------------------------
-- 2. Intentional type analysis
-- it's essentially doing the runtime type checking trick.

class Show'Intensional a where
  show'intensional :: a -> String

instance Show'Intensional Int where
  show'intensional = int_to_string

instance Show'Intensional Bool where
  show'intensional = bool_to_string

-- this compiles to

-- This is a little circular since we're still using typeclass. Typeclass here
-- can be thought as compiler builtin rtti predicates.
show'intensionalCompiled :: Typeable a => a -> String
show'intensionalCompiled x =
  case cast x :: Maybe Bool of
    Just v -> bool_to_string v
    Nothing ->
      case cast x :: Maybe Int of
        Just v -> int_to_string v
        Nothing -> error "no type resolution"

-- call site is essentially the same, as the show function already can
-- dispatch base on all situations, we only need one function.
test_show'intensional :: String
test_show'intensional = show'intensionalCompiled True

-- Typeable again can be build into compiler.
print'intensional :: Typeable a => a -> IO ()
print'intensional = putStrLn . show'intensionalCompiled

-- again no problem.
print_ints'intensional :: IO ()
print_ints'intensional = do
  traverse print'intensional [1 :: Int, 2, 3]
  return ()

-- We no longer have parametricity since the intensional version show doesn't
-- support all types, some runtime input will an exception.
--
-- print :: a -> String looks like it's paramtric polymorphic, but it is not.
-- It doesn't work for types other then int and bool beacuse we only have
-- typeclasses for them.
--
-- NOTE: it's not always possible to do runtime type checking. Sometimes you
-- need to add type information for all values for them to be able to be type
-- checked.

