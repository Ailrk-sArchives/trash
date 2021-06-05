{-# LANGUAGE RankNTypes #-}
module Cat.Monads1 where

import           Data.Function
import           Data.Maybe


{-@ Using monad to extend a simple interpreter.
    Monads with only basic algebraic data type and functions.
    because there is no typeclass, we can't overload >>=, instead
    each monad has it's own bind.

    A monad can be thought as a triple (M, unitM, bindM), where M is the
    constructor.
@-}


-- you can convert this to scott encoding to make the entire implementation only
-- rely on function
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
-- monad has two operations bind and unit.
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

-- | For error monad, we have an additional operation errorE.
-- | We call this method to signal the error.
errorE s = Error s
------------------------------------------------


-- PE monad: Position + Error  -----------------
--
-- Mannually crank up a monad transformer.
-- PE :: Position -> E a
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

-- lifting
errorPE s = PE $ \p -> errorE (s ++ ", Position:" ++ show p)
------------------------------------------------


-- SPE monad: Position + Error + State Transformer
-- SPE :: s -> Position -> E (a, s)
newtype SPE s a = SPE { unSPE :: (s -> PE (a, s)) }

unitSPE a = SPE $ \s -> unitPE (a, s)

bindSPE :: SPE s a -> (a -> SPE s b) -> SPE s b
m `bindSPE` k = SPE $ \s ->
  (unSPE m $ s) `bindPE` (\(a, s') ->
    (unSPE (k a) $ s') `bindPE` (\(b, s'') ->
      unitPE (b, s'')))

-- test1 (seems work)
-- to unwrap a monad transformer, we need to do it outside in.
t1 = (unPE (unSPE t "initial state")) (Pos 0 0)
  where
    t = (unitSPE 1) `bindSPE` (\x ->

      -- sequence. The monadic function doesn't use the input, but an effect is performed
      -- by putSPE implicitly.
      -- Effect is accessible within the funciton now.
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
resetSPE :: Position -> SPE s x -> SPE s x
resetSPE pos (SPE m) = SPE $ \s -> resetPE pos (m s)

------------------------------------------------

-- OSPE monad: Position + Error + State Transformer + Output
-- OSPE :: s -> Position -> E ((String, a), s)

newtype OSPE s a = OSPE { unOSPE :: SPE s (String, a) }

unitOSPE a = OSPE . unitSPE $ ("", a)

bindOSPE :: OSPE s a -> (a -> OSPE s b) -> OSPE s b
m `bindOSPE` k = OSPE $
  (unOSPE m) `bindSPE` (\(out, a) ->
    unOSPE (k a) `bindSPE` (\(out1, b) ->
      unitSPE (out ++ out1, b)))


-- Output monde

outOSPE a = OSPE .unitSPE $ (show a ++ "; ", unitSPE ())


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
resetOSPE pos (OSPE m) = OSPE $ resetSPE pos m

--   (o, resetSPE pos m)

t3 = (unPE (unSPE (unOSPE t) "initial state")) (Pos 0 0)
  where
    t = unitOSPE 1 `bindOSPE` (\x ->
      putOSPE "new state" `bindOSPE` (\_ ->
      modifyOSPE (++ ", appened with this") `bindOSPE` (\_ ->
      outOSPE ("new message" ++ show x) `bindOSPE` (\_ ->
      outOSPE ("another message") `bindOSPE` (\_ ->
      errorOSPE "done")))))

t4 = (unPE (unSPE (unOSPE t) "initial state")) (Pos 0 0)
  where
    t = unitOSPE 1 `bindOSPE` (\x ->
      putOSPE "new state" `bindOSPE` (\_ ->
      modifyOSPE (++ ", appened with this") `bindOSPE` (\_ ->
      outOSPE ("new message" ++ show x) `bindOSPE` (\_ ->
      outOSPE ("another message") `bindOSPE` (\_ ->
      unitOSPE "done")))))


------------------------------------------------


type M = OSPE Int
unitM = unitOSPE
bindM = bindOSPE

-- additional operation for all monads
--
-- functor
mapForM :: (a -> b) -> M a -> M b
mapForM f m = m `bindM` (\a -> unitM (f a))

-- join nested monads.
-- >>= once consume one layer.
joinM :: M (M a) -> M a
joinM z = z `bindM` id


{-@ Monad Laws
    Left identity:
      pure a >>= k = k a

    Right identity:
      m >>= pure = m

    Associativity:
      m >>= (\a -> k a >>= (\b -> k b))
      =
      (m >>= \a -> k a) >>=)\b -> k b

    A law abiding monad compose!
@-}
t5 = (unPE (unSPE (unOSPE t) "initial state")) (Pos 0 0)
  where
    -- by left identity law: (pure a) >>= k = k a
    -- in practice left identify means if you just want to shove a value into a
    -- monadic computation, just use the mnadic function is enough, no bind is needed.
    t = 1 & (\x ->
      putOSPE "new state" `bindOSPE` (\_ ->
      modifyOSPE (++ ", appened with this") `bindOSPE` (\_ ->

      -- by associativity law
      -- in practice it means you can carve an expression in the middle, define it
      -- somewhere else, and refer to it in the big expression.
      (outOSPE ("new message" ++ show x) `bindOSPE` (\_ ->
      outOSPE ("another message")) `bindOSPE` (\_ ->
      unitOSPE "done")))))



--------- language -----------------------------------
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

-- effecful operations
eval (Out u) e = eval u e `bindM` (\a ->
                 outOSPE a `bindM` (\_ -> unitM a))
eval (At p t) e = resetOSPE p (eval t e)

lookup' :: Name -> Env -> M Value
lookup' x [] = errorOSPE "not in environment"
lookup' x ((y, b):e)
  | x == y = unitM b
  | otherwise = lookup' x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM $ Num (i + j)
add _ _             = errorOSPE "Arguments need to be Num"

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply _ _       = errorOSPE "wrong application"





{-@ Continuation monad

    Continuation Passing style was first developede with denotational
    semantics.

    CPS provides control over the execution order of a program.
@-}

-- what is continuation?
--  a continuation takes a function that accepts a value a, and return the
-- result of the function.
newtype K a = K { cont :: forall r. (a -> r) -> r }

-- how to lift value into continuation?
--  create a fun
unitK a = K (\c -> c a)

bindK :: K a -> (a -> K b) -> K b
(K m) `bindK` k = K (\c -> m (\a -> (cont $ k a) c))



{-@ Monad and CPS @-}


