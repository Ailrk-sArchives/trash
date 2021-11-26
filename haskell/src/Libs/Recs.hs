{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module Libs.Recs where

import GHC.Records
import Data.Kind
import Data.Proxy


data Foo = Foo { name :: String
               , age :: Int
               }
               deriving Show

t1 = getField @"name" (Foo "Jerry" 10)

-- >>> t1
-- "Jerry"


t2 = foo { age = (+1) (getField @"age" foo) }
  where
    foo = (Foo "Jerry" 10)

-- >>> t2
-- Foo {name = "Jerry", age = 11}



-- virtual fields
instance HasField "name_tag" Foo String where
  getField = name <> const "-" <> show . age

-- >>> getField @"name_tag" (Foo "Jerry" 10)
-- "Jerry-10"


data Record (xs :: [(k, Type)]) where
  Nil :: Record '[]
  Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

instance HasField x (Record ('(x, a) ': xs)) a where
  getField (Cons _ v _) = v

instance {-# OVERLAPS #-} HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
  getField (Cons _ _ r) = getField @x r

r :: Record '[ '("name", String) ]
r = Cons Proxy "R" Nil

t3 = getField @"name" r

-- >>> t3
-- "R"

-----
-- new record dot syntax
