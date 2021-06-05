module Cat.Monads1 where

import           Data.Maybe


{-@ Using monad to extend a simple interpreter.
    Monads with only basic algebraic data type and functions.
    because there is no typeclass, we can't overload >>=, instead
    each monad has it's own bind.
@-}

data Position = Pos { col :: Int, line :: Int } deriving (Eq, Show)

-- We are describing effect with functons and data types only, it's clear to
-- see where the verbosity of monad appears.
--
-- The most annoying part is you always need to lift an operation to the right
-- level of the monad stack.
--
-- Typeclass already removed a lot of boilerplates, but there are still a lot of
-- extra code needs to be written.

-- starts from the identity monad.
-- We can gradually adding more functionalities.
--
-- I monad: Identify --------------------------
data I a = I a

-- Monad Instance --
unitI :: a -> I a
unitI = I

bindI :: I a -> (a -> I b) -> I b
bindI (I a) f =  f a
------------------------------------------------

-- E monad: Error ------------------------------
data E a = Success a | Error String deriving (Eq, Show)


-- Monad Instance --
unitE a = Success a
(Success a) `bindE` k = k a
(Error e) `bindE` _   = Error e

errorE s = Error s

showE (Success a) = "Success: " ++ show a
showE (Error e)   = "Error: " ++ e
------------------------------------------------


-- PE monad: Position + Error  -----------------
newtype PE a = PE { unPE :: (Position -> E a) }

-- Monad Instance --
unitPE a = PE $ \_ -> unitE a

bindPE :: PE a -> (a -> PE b) -> PE b
m `bindPE` k = PE $ \p ->
  (unPE m) p `bindE` (\x ->
    (unPE (k x)) p)

-- Position.
-- | reset position
resetPE :: Position -> PE x -> PE x
resetPE pos (PE m) = PE (\p -> m pos)

errorPE s = PE $ \p -> errorE (s ++ ", Position:" ++ show p)
------------------------------------------------



-- SPE monad: Position + Error + State Transformer
newtype SPE s a = SPE { unSPE :: (s -> PE (a, s)) }

unitSPE a = SPE $ \s -> unitPE (a, s)

bindSPE :: SPE s a -> (a -> SPE s b) -> SPE s b
m `bindSPE` k = SPE $ \s ->
  (unSPE m $ s) `bindPE` (\(a, s') ->
    (unSPE (k a) $ s') `bindPE` (\(b, s'') ->
      unitPE (b, s'')))

-- test1 (seems work)
t1 = (unPE (unSPE t "initial state")) (Pos 0 0)
  where
    t = (unitSPE 1) `bindSPE` (\x ->
      putSPE "state" `bindSPE` (\_ ->
      modifySPE (\s -> "modified state") `bindSPE` (\_ ->
      errorSPE ("It just throws an error for some reason"))))

t2 = (unPE (unSPE t "initial state")) (Pos 0 0)
  where
    t = (unitSPE 1) `bindSPE` (\x ->
      putSPE "state" `bindSPE` (\_ ->
      modifySPE (\s -> "modified state") `bindSPE` (\_ ->
      unitSPE 10)))



-- State specifics

modifySPE :: (s -> s) -> SPE s ()
modifySPE f = SPE $ \s -> unitPE ((), f s)

putSPE :: s -> SPE s ()
putSPE s' = SPE $ \s -> unitPE ((), s)

getSPE :: SPE s s
getSPE = SPE $ \s -> unitPE (s, s)

-- lifting:
-- error
errorSPE a = SPE $ \s -> errorPE a

-- reset
-- resetSPE pos (SPE m) = SPE $ \s -> let (m', s') = m s
--                                     in (resetPE pos m' , s)


------------------------------------------------

-- OSPE monad: Position + Error + State Transformer + Output

newtype OSPE s a = OSPE { unOSPE :: SPE s (String, a) }

unitOSPE a = OSPE . unitSPE $ ("", a)

bindOSPE :: OSPE s a -> (a -> OSPE s b) -> OSPE s b
m `bindOSPE` k = OSPE $
  (unOSPE m) `bindSPE` (\(out, a) ->
    unOSPE (k a) `bindSPE` (\(out1, b) ->
      unitSPE (out ++ out1, b)))


-- Output monde
outOSPE a = (show a ++ "; ", unitSPE ())


-- lifting, eh...
-- State specifics
modifyOSPE :: (s -> s) -> OSPE s ()
modifyOSPE f = OSPE $
  modifySPE f `bindSPE` (\_ ->
  unitSPE ("", ()))


putOSPE :: s -> OSPE s ()
putOSPE s = OSPE $
  putSPE s `bindSPE` (\_ ->
  unitSPE ("", ()))


getOSPE :: OSPE s s
getOSPE = OSPE $
  getSPE `bindSPE` (\s ->
    unitSPE ("", s))


-- more lifting:
-- error
errorOSPE a = OSPE $ errorSPE a

-- reset
resetOSPE pos (OSPE m) = OSPE $
  undefined

--   (o, resetSPE pos m)

------------------------------------------------

type M = I
unitM = unitI
bindM = bindI

-- type M = OSPE Int

-- bindM = bindOSPE
-- unitM = unitOSPE



type Name = String

data Term  = Var Name
           | Cont Int
           | Add Term Term
           | Lam Name Term
           | App Term Term
           | At Position Term
           | Out Term

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




{-@ Monad Laws guarantees monad compose. @-}

{-@ Continuation monad
@-}


{-@ Monad and CPS @-}


