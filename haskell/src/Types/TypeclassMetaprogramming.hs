{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module Types.TypeclassMetaprogramming where
import qualified Data.Map               as Map
import qualified Data.Vector            as Vector
import           Data.Void
import           Numeric.Natural

import           Control.Exception.Base (assert)
import           Data.Kind
import           System.IO

import           Data.Type.Equality

-- https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/

-------------------------------------------------------------------------------
-- typeclass as function from type to term
-- use compile time information to generate runtime behavior.

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"

-- type application allows us to define TypeOf for void.
instance TypeOf Void where
  typeOf = "Void"

instance TypeOf a => TypeOf [a] where
  typeOf = "[" ++ typeOf @a ++"]"

-------------------------------------------------------------------------------
-- type level interpreter
data Nat = Z | S Nat

class ReifyNat (a :: Nat) where
  reifyNat :: Natural

instance ReifyNat 'Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat ('S a) where
  reifyNat = 1 + (reifyNat @a)

-------------------------------------------------------------------------------
-- overlapping instances.
-- this works somehow like template specialization in c++.
-- when there is overlapping instances, it choose the more specific instance

class IsUnit a where
  isUnit :: Bool

instance {-# OVERLAPPING #-} IsUnit () where
  isUnit = True

instance IsUnit a where
  isUnit = False

guardUnit :: forall a . IsUnit a => a -> Either String a
guardUnit x = case isUnit @a of
                True  -> Left "No unit"
                False -> Right x

-- Note this doesnt work because the instance needs to be picked at the compile
-- time, but we don't know which one to pick because type of a is instantiated
-- at runtime.
-- guardUnit :: forall a . a -> Either String a
-- guardUnit x = case isUnit @a of
--                 True -> Left "no unit"
--                 False -> Right x


-------------------------------------------------------------------------------
-- type families, function from type to type.

type family Sum a b where
  Sum Z b = b
  Sum (S a) b = S (Sum a b)

n1 = reifyNat @(Sum (S (S Z)) (S (S (S Z))))

-------------------------------------------------------------------------------
-- e.g flattern
-- concat with arbitrary depth.

type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a] = a

class Flatten a where
  flatten :: a -> [ElementOf a]

-- base case
instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat

n2 = flatten [[[1 :: Integer, 2], [3, 4]], [[5, 6], [7, 8]]]

-------------------------------------------------------------------------------
-- type class as compile type code generation.
-- in this case it works like templates.

-- flatten @[[Int]] ->  id . concat . concat
n3 = flatten @[[Int]] $ [[1, 2], [3, 4]]

-------------------------------------------------------------------------------
-- open type family written within type class.

class HasKey a where
  type Key a
  hasKey :: Key a -> a -> Bool

instance HasKey (Vector.Vector a) where
  type Key (Vector.Vector a) = Int
  hasKey i vs = i >= 9 && i < Vector.length vs

instance Ord k => HasKey (Map.Map k v) where
  type Key (Map.Map k v) = k
  hasKey = Map.member

-------------------------------------------------------------------------------
-- datatype generic programming

type Username = String
type Password = String
type PublicKey = String

-- Sum type version
data Authentication = AuthBaisc Username Password
                    | AuthSSH PublicKey

-- this just counting number of elements in a adt.
class GNumFields a where
  gnumFields :: a -> Natural

instance {-# OVERLAPPING #-} GNumFields () where
  gnumFields _ = 0

instance GNumFields a where
  gnumFields _ = 1

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b

authGenerialize :: Authentication -> Either (Username, Password) PublicKey
authGenerialize (AuthBaisc u p) = Left (u, p)
authGenerialize (AuthSSH p)     = Right p

numFieldsAuth :: Authentication -> Natural
numFieldsAuth = gnumFields . authGenerialize

n4 = numFieldsAuth (AuthSSH "asd")
n5 = numFieldsAuth (AuthBaisc "asd" "asd")

-------------------------------------------------------------------------------
-- Define a generic  NumFields that works on any types.
-- The idea is to convert a type to a isomorphic representation that we can
-- work with.
-- If we have typeclass that works on the transformed representation, we can
-- do various things with the new representation and map it back when we
-- are finished

class Generik1 a where
  type Rep1 a
  genericize1 :: a -> Rep1 a

-- Rep maps the type to it's representation type.
-- in our case we want to map it to either or tuple so we can count with GNumFields.
instance Generik1 Authentication where
  type Rep1 Authentication = Either (Username, Password) PublicKey
  genericize1 (AuthBaisc user pass) = Left (user, pass)
  genericize1 (AuthSSH key)         = Right key

-- we want (Rep a) available for GNumFields
numFields :: (Generik1 a, GNumFields (Rep1 a)) => a -> Natural
numFields = gnumFields . genericize1

n6 = numFields (AuthBaisc "asd" "asd")
n7 = numFields (AuthSSH "asd")

-------------------------------------------------------------------------------
-- 1. this type will give wrong numfields because Rep a will be two nested
--    either, one is only a representation for the sum type.
--    To solve this, we can wrap each leaf with a new type Leaf.
data Foo = A (Either Int String) | B (Char, Bool)
data Boool = FF | TT
newtype Leaf a = Leaf { getLeaf :: a }

class Generik2 a where
  type Rep2 a
  genericize2 :: a -> Rep2 a

instance Generik2 Authentication where
  type Rep2 Authentication = Either (Leaf Username, Leaf Password) (Leaf PublicKey)
  genericize2 (AuthBaisc user pass) = Left (Leaf user, Leaf pass)
  genericize2 (AuthSSH key)         = Right (Leaf key)

instance Generik2 Foo where
  type Rep2 Foo = Either (Leaf (Either Int String)) (Leaf (Char, Bool))
  genericize2 (A x) = Left (Leaf x)
  genericize2 (B x) = Right (Leaf x)

instance Generik2 Boool where
  type Rep2 Boool = Either () ()
  genericize2 FF = Left ()
  genericize2 TT = Right ()

numFields2 :: Generik2 a => GNumFields (Rep2 a) => a -> Natural
numFields2 = gnumFields . genericize2

n8 = numFields2 (A (Left 1))
n9 = numFields2 (B ('a', True))
n10 = numFields2 (AuthBaisc "asd" "asd")
n11 = numFields2 TT

------------------------------------------------------------------------------
-- dependent types

-- Note: a has kind *, so it will accpet whatever types. If the pattern
-- matching fails, it will get stuck.
-- For example, BadNot Char will return BadNot Char instead of evaluate further
--
-- DataKind promotes types to new kinds and constructors to types, which give
-- us more elements to work with at the type level.
type family BadNot a where
  BadNot 'True = 'False
  BadNot 'False = 'True

type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

------------------------------------------------------------------------------
-- runtime information -> compile time type
-- GADT and proof terms

data WhatIsIt a where
  ABool :: WhatIsIt Bool
  AInt :: WhatIsIt Int

-- the type of x is determined by tye constructor.
-- which constructor is passed is a runtime behavior. But we can infer the type
-- of the type parameter with GDAT
--
-- Think GADT as a proof of type equalities.
-- ABool => a ~ Bool
-- AInt => a ~ Int
doSometing :: WhatIsIt a -> a -> a
doSometing ABool x = not x
doSometing AInt x  = x + 1

-- GADT with type level list.
infixr 5 `HCons`

-- hetergenous list
-- we defined two propositions, a term level value is a proof of the proposition
data HList as where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

-- note because we specifically say the input is of type HList (a ': as), GHC
-- knows the list will not be empty.
headHList :: HList (a ': as) -> a
headHList (x `HCons` _) = x

type family Head as where
  Head (HList '[]) = 'Nothing
  Head (HList (a ': as)) = 'Just a

type family FromJust (a :: Maybe b) where
  FromJust (Just x) = x

-- note HList only takes kinds with *, it doesn't accept types promoted from
-- data constructors, because they have different kinds.
type T1 = Head (HList '[Bool, Char])

-- but for this case we need to handle the empty case
-- headHList' :: Head as => HList as -> Maybe a
-- headHList' (x `HCons` _) = Just x
-- headHList' HNil = Nothing

n12 = True `HCons` "hellow" `HCons` 42 `HCons` HNil

------------------------------------------------------------------------------
-- proofs that works together.
data OneToThree a b c as where
  One :: OneToThree a b c '[a]
  Two :: OneToThree a b c '[a, b]
  Three :: OneToThree a b c '[a, b, c]

sumUpToThree :: OneToThree Int Int Int as -> HList as -> Int
sumUpToThree One (x `HCons` HNil)                       = x
sumUpToThree Two (x `HCons` y `HCons` HNil)             = x + y
sumUpToThree Three (x `HCons` y `HCons` z `HCons` HNil) = x + y + z

------------------------------------------------------------------------------
-- we say: Give me a Even as, I can give you a Even (a ': b ': as) which has
-- at least two elements. the argument goes over inductively until hit the
-- base case. So this term proofs that the list is always even.

data Even as where
  EvenNil :: Even '[]
  EvenCons :: Even xs -> Even (a ': b ': xs)

-- grab two elements and pair them up.
type family PairUp as where
  PairUp '[] = '[]
  PairUp (a ': b ': xs) = (a, b) ': PairUp xs

-- pairup elements in a HList
-- Even as proofs that as must be even
-- note how it's passed as the first paramter
pairUp' :: Even as -> HList as -> HList (PairUp as)
pairUp' EvenNil HNil                             = HNil
pairUp' (EvenCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp' even xs

n14 = pairUp' (EvenCons EvenNil) (() `HCons` "foo" `HCons` HNil)
n15 = pairUp' (EvenCons $ EvenCons EvenNil)
             (True `HCons` 'a' `HCons` () `HCons` "foo" `HCons` HNil)
n16 = pairUp' (EvenCons $ EvenCons $ EvenCons EvenNil)
             (False `HCons` 1 `HCons` True `HCons` 'a' `HCons` () `HCons` "foo" `HCons` HNil)

------------------------------------------------------------------------------
-- What if we don't want to pass Even proof term all the time?
-- remeber typeclass is type predicate + type to term funcction
-- this is a simple application of the tmp technique.

class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenCons evenProof


-- as = '[Bool, Bool, Bool, Bool],
-- evenProof
-- = EvenCons (evenProof :: '[Bool, Book])
-- = EvenCons (EvenCons (evenProof :: '[])))
-- = EvenCons (EvenCons EvenNil)
-- Even is used to construct proof recursively.
-- The purpose of evenProof is to be a proof term that can recursively generate
-- new EvenCons base on the type as.
pairUp :: IsEven as => HList as -> HList (PairUp as)
pairUp = go evenProof
  where
    go :: Even as -> HList as -> HList (PairUp as)
    go EvenNil HNil                             = HNil
    go (EvenCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` go even xs

-- now the proof is implicit
n17 = pairUp (() `HCons` "foo" `HCons` HNil)

------------------------------------------------------------------------------
-- GADT vs type family

-- EvenPairsCons EvenPairsNil :: '[a, b] [(a, b)]
-- EvenPairsCons (EvenPairsCons EvenPairsNil) :: '[a, b, c, d] [(a, b), (c, d)]
data EvenPairs as bs where
  EvenPairsNil :: EvenPairs '[] '[]
  EvenPairsCons :: EvenPairs xs ys -> EvenPairs (a ': b ': xs) ((a, b) ': ys)

-- what is a function? A function is a mapping, in a set theory pov it's an
-- ordered pair relates input and output.
-- If we can construct a proof that relates it's input and output, we don't
-- really need type family anyway.
-- Here, EvenPairs takes an input type as, and the output tupe is bs. The
-- relation between as and bs is defined in the GADT. We can just us bs as if
-- we have a type famliy PairUp.
pairUp1 :: EvenPairs as bs -> HList as -> HList bs
pairUp1 EvenPairsNil HNil = HNil
pairUp1 (EvenPairsCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp1 even xs

------------------------------------------------------------------------------
-- Type family to achieve the smae
-- I find type family is much easier to write...

-- a type level predicate that constraint a list to be even elements.
type family IsEvenTF as :: Constraint where
  IsEvenTF '[] = ()
  IsEvenTF (_ ': _ ': xs) = IsEvenTF xs

type family FirstTwo as where
  FirstTwo '[] = '[]
  FirstTwo (a ': b ': _) = a ': b ': '[]

-- this pattern is incomplete intensionally, can I turn the warnning off for
-- just this type family?
firstTwo :: IsEvenTF as => HList as -> HList (FirstTwo as)
firstTwo HNil                    = HNil
firstTwo (x `HCons` y `HCons` _) = x `HCons` y `HCons` HNil

pairUp2 :: IsEvenTF as => HList as -> HList (PairUp as)
pairUp2 HNil                     = HNil
pairUp2 (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp2 xs

n18 = firstTwo (1 `HCons` "a" `HCons` 3 `HCons` 'a' `HCons` HNil)
n19 = pairUp2 (1 `HCons` "a" `HCons` 3 `HCons` 'a' `HCons` HNil)

-- GADT vs type family:
-- GADT + proofterm can achieve similar effect as type family, but GADT more
-- precise constraints.

------------------------------------------------------------------------------
-- Guided type inference

class UnitList as where
  unitList :: HList as

instance UnitList '[] where
  unitList = HNil

-- How does GHC resolve which instance to use?
-- First it match instance head, if it matches it will try to solve the
-- constraint imposed from the instance context.
-- For example
-- instance head UnitList (() ': as) will only pick elements with first element
-- to be a unit.
-- while UnitList (a ': as) will be picked for all lists.
-- this constriant the list can only be a list of units
--
-- If we want a finer control for type inference, we can move information in
-- the instance head to instance context and hand write constraints.
instance (a ~ (), UnitList as) => UnitList (a ': as) where
  unitList = () `HCons` unitList

-- create a HList with 3 units without defining it explicitly.
-- TMP generate the term for us.
n20 = unitList :: HList '[(), (), ()]

unsingleton :: HList '[a] -> a
unsingleton (x `HCons` HNil) = x

n21 = unsingleton (unitList :: HList '[()])
n22 = unsingleton unitList

------------------------------------------------------------------------------
-- Subtyping constraints.
-- In haskell we don't have subtyping, beacuse it doesn't support type inferece.
-- instead most ppl use polymorphism to get around with it.
-- Subtyping is not a equivalence relation, so we can't simpmly run unification
-- algorithm on different types.

-- Something can be a input, or an output.  both is a subtype of both.
data GQLKind
  = Both
  | Input
  | Output

-- how it may be used
data GQLType k where
  TScalar :: GQLType 'Both
  TInputObject :: Int -> GQLType 'Input
  TIObject :: Int -> GQLType 'Output

-- proof subkinding relationship
-- To proof the relationship we give an instance of value in the propositrion
data SubKind (k1 :: GQLKind) (k2 :: GQLKind) where
  KRefl :: SubKind k k    -- two k must be the same
  KBoth :: SubKind 'Both k -- whatever k will do.

-- Use typeclass to help us to generate proof.
class IsSubKind k1 k2 where
  subKindProof :: SubKind k1 k2

-- ignore k
instance IsSubKind 'Both k where
  subKindProof = KBoth

-- only accept Input
instance (k ~ 'Input) => IsSubKind 'Input k where
  subKindProof = KRefl

instance (k ~ 'Output) => IsSubKind 'Output k where
  subKindProof = KRefl

data GQLParser (k :: GQLKind) a where
  GQLParser :: a -> GQLParser k a

nullable :: IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
nullable = undefined

n23 = nullable (GQLParser @Int @'Input 1)
n24 = nullable (GQLParser @Int @'Both 1)

-- same thing.
n25 = (nullable psin, nullable psout)
  where
    psin :: GQLParser 'Input Int
    psin = undefined

    psout :: GQLParser 'Both Int
    psout = undefined

data PValue = InputValue | SelectionSet

type family ParserInput k where
  ParserInput 'Both = InputValue
  ParserInput 'Input = InputValue
  ParserInput 'Output = SelectionSet

-- Refl gives a proof of a equals to b. w
-- :~: itself is a type. ~ is an operator for imposing type equality constraint
-- Use Refl to write a resuable proof.
inputParserInput :: forall k. IsSubKind k 'Input => ParserInput k :~: InputValue
inputParserInput = case subKindProof @k @'Input of
                    KRefl -> Refl
                    KBoth -> Refl

-- use the reusable proof term to redefine nullable
nullable1 :: forall k a. IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
nullable1 parser = case inputParserInput @k of
                     Refl -> undefined

------------------------------------------------------------------------------
run :: IO ()
run = do
  assert (n1 == 5) (return ())
  assert (n2 == [1..8]) (return ())
  assert (n3 == [1..4]) (return ())
  assert (n4 == 1) (return ())
  assert (n5 == 2) (return ())
  assert (n6 == 2) (return ())
  assert (n7 == 1) (return ())
  assert (n8 == 1) (return ())
  assert (n9 == 1) (return ())
  assert (n10 == 2) (return ())
  assert (n11 == 0) (return ())
