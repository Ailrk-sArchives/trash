module Cat.Monads1 where

import           Data.Maybe


{-@ Using monad to extend a simple interpreter.
    Monads with only basic algebraic data type and functions.
    because there is no typeclass, we can't overload >>=, instead
    each monad has it's own bind.
@-}

data Position = Pos { col :: Int, line :: Int } deriving (Eq, Show)


-- starts from the identity monad.
-- We can gradually adding more functionalities.
--
-- I monad: Identify --------------------------
data I a = I a

unitI :: a -> I a
unitI = I

bindI :: I a -> (a -> I b) -> I b
bindI (I a) f =  f a
------------------------------------------------

-- E monad: Error ------------------------------
data E a = Success a | Error String

errorE s = Error s

unitE a = Success a

(Success a) `bindE` k = k a
(Error e) `bindE` _   = Error e

showE (Success a) = "Success: " ++ showval a
showE (Error e)   = "Error: " ++ e
------------------------------------------------


-- PE monad: Position + Error  -----------------
newtype P a = P (Position -> E a)


unitP = P . const unitE
m `bindP` k = P $ \p -> m p `bindE` (\x -> k x p)

-- lift error.
errorP s = P $ \p -> errorE (show p ++ ": " ++ s)

-- undate position.
resetP :: Position -> P x -> P x
resetP pos (P m) = P (\p -> m pos)

------------------------------------------------


-- PES monad: Position + Error + State Transformer
newtype S s a = S (s -> (a, s))

unitS a = S $ \s -> (a, s)
m `bindS` k = \s -> let (a, s') = m s
                        (b, s'') = k a s'
                     in (b, s'')

tickS :: S s ()
tickS = undefined

------------------------------------------------

type M = I

bindM = bindI
unitM = unitI


type Name = String

data Term  = Var Name
           | Cont Int
           | Add Term Term
           | Lam Name Term
           | App Term Term
           | At Position Term

data Value = Wrong
           | Num Int
           | Fun (Value -> I Value)

type Env = [(Name, Value)]

showval :: Value -> String
showval Wrong   = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

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

-- eval (At p t) e = resetP p (eval t e)

lookup' :: Name -> Env -> M Value
lookup' x [] = unitM Wrong
lookup' x ((y, b):e)
  | x == y = unitM b
  | otherwise = lookup' x e

add :: Value -> Value -> I Value
add (Num i) (Num j) = unitM $ Num (i + j)
add _ _             = unitM Wrong

apply :: Value -> Value -> I Value
apply (Fun k) a = k a
apply _ _       = unitM Wrong


