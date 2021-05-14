{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Types.Inferences where

-- https://www.youtube.com/watch?v=ZiGIBU0haOk

-- type equality is useful for associate types of type families
-- because it's hard to infer whether two associate types should be
-- equal

{-@ injectivity @-}
-- all type constructors are injective
injective :: f a ~ g b => a -> b
injective = id

-- an example of type equality
injMaybe :: forall a b. Maybe a ~ Maybe b => a -> b
injMaybe = id

-- supply type for type application.
injective' = injective @Maybe

{-@ generativity @-}
generativity :: f a ~ g b => f c -> g c
generativity = id

{-@ if two types satisfy injectivity and gnerativity, then
    we say they are matchable.
@-}

{-@ Now assume we have a type family like this:
    by injectivity, we have
    Maybe a ~ Maybe b => a ~ b

    but for associative type family, we don't want this:
    DBValue a ~ DBValue b => a ~ b

    otherwise we will get into the situation that
    DBValue UUID ~ DBValue Password => UUID ~ Password
    which makes little sense.

   So type familiies is no injective. (saturated)
@-}

data UUID = UUID
data Username = Username
data Password = Password

class DB a where
  type DBValue a
  toDB :: a -> DBValue a

instance DB UUID where
  type instance DBValue UUID = Int
  toDB _ = 1

instance DB Username where
  type instance DBValue Username = String
  toDB _ = "UserName"

instance DB Password where
  type instance DBValue Password = String
  toDB _ = "Password"


type User = '[UUID, Username, Password]

data HList :: [*] -> * where
  Nil :: HList '[]
  (:>) :: t -> HList ts -> HList (t ': ts)

infixr :>

jotaro :: HList User
jotaro = UUID :> Username :> Password :> Nil

-- track matchability in kind
-- Maybe :: * -> *  "matchable"
-- DBType :: * ~> *  "unmatchable"
--
-- We can't use DBType for Map because it's saturated.

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

dbUser :: HList User -> HList (Map Maybe User)
dbUser = undefined
