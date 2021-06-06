module Cat.MonadsCPS.MonadInterp where

import Cat.MonadsCPS.Monads

--------- language -----------------------------------
type Name = String

data Term  = Var Name
           | Cont Int
           | Add Term Term
           | Lam Name Term
           | App Term Term
           | Cout
           | At Position Term
           | Out Term

data Value = Wrong
           | Num Int
           | Fun (Value -> M Value)

type Env = [(Name, Value)]

showval :: Value -> String
showval Wrong   = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

instance Show Value where
  show = showval

eval :: Term -> Env -> M Value
eval (Var x) e = lookup' x e

eval (Cont i) e = unitM (Num i)

eval (Add u v) e = eval u e `bindM` \a ->
                   eval v e `bindM` \b ->
                   add a b
eval (Lam x v) e = unitM . Fun
                 $ \a -> eval v ((x, a): e)
eval (App t u) e = eval t e `bindM` \f ->
                   eval u e `bindM` \a ->
                   apply f a

eval (Cout) e = getOSPE `bindM` (\s -> unitM (Num s))

-- effecful operations

-- print the value to the output, and return the value unchanged.
eval (Out u) e = eval u e `bindM` (\a ->
                 outOSPE a `bindM` (\_ -> unitM a))

-- modify the position info
eval (At p t) e = resetOSPE p (eval t e)

lookup' :: Name -> Env -> M Value
lookup' x [] = errorOSPE "not in environment"
lookup' x ((y, b):e)
  | x == y = unitM b
  | otherwise = lookup' x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = tickM `bindM` (\_ -> unitM $ Num (i + j))
add _ _             = errorOSPE "Arguments need to be Num"

apply :: Value -> Value -> M Value
apply (Fun k) a = tickM `bindM` (\_ -> k a)
apply _ _       = errorOSPE "wrong application"


-- test
t' = unM 0 $ eval (At (Pos 1 2) (App (Cont 1) (Cont 2))) []

t'' = unM 0 $ eval (Out
  (Add
    (Add (Out (Cont 1)) (Out(Cont 2)))
    (Add (Out Cout) (Out Cout)))) []

t''' = unM 0 $ eval (Out (Cont 1)) []

t6 = unM 0 $ eval (App (Lam "x" (Add (Var "x") (Var "x"))) (Cont 1)) []


{-@ Call by name and monad @-}
-- argument of the function is just a value.
foo :: a -> m b
foo = undefined

-- turn argument of the function into a computation
bar :: m a -> m b
bar = undefined

