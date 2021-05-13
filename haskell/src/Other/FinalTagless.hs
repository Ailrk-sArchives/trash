{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}

module Other.FinalTagless where

-- expression problem.
-- adt is extensible on adding method but closed on adding datatype
-- subtypig is extensible on adding datatype but closed on adding method?
-- no.
-- what about both.


-- this is the original type
data Expr' a where
  Lit :: Int -> Expr' Int
  Add :: Expr' Int -> Expr' Int -> Expr' Int

-- this is the final tagless version.
{-@ final tagless use function ass data constructor
@-}

-- basic
class Expr a where
  lit :: Int -> a
  add :: a -> a -> a

instance Expr Int where
  lit = id
  add = (+)

instance Expr String where
  lit = show
  add a b = a ++  " + " ++ b

-- extend
class Minus a where
  minus :: a -> a -> a

instance Minus Int where
  minus = (-)

instance Minus String where
  minus a b = a ++ " - " ++ b

-- now just add these two constrains together

expr1 :: Expr a => Minus a => a
expr1 = add (lit 12) (minus (lit 32) (lit 3))

expr1AsInt :: Int
expr1AsInt = expr1 + 12

expr1AsString :: String
expr1AsString = expr1
