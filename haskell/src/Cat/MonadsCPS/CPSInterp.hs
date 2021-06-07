module Cat.MonadsCPS.CPSInterp where

import Cat.MonadsCPS.Monads

{-@ Continuation monad

    Continuation Passing style was first developede with denotational
    semantics.

    CPS provides control over the execution order of a program.
@-}

-- what is continuation?
--  a continuation takes a function that accepts a value a, and return the
-- result of the function.
newtype K r a = K { cont :: (a -> r) -> r }

-- how to lift value into continuation?
--  create a fun
unitK a = K (\c -> c a)

-- bind for CPS monad:
-- 1. creates a new K with parameter c being the current continuation
-- 2. evaluate m (call m with function (\a -> cont $ k a) c)
-- 3. bind the result of m to paramter of it's continuation (paramter a)
-- 4. evaluate (k a) and bind it's result to c. (continue on c)
--
-- We bind (K m) to k:
-- 1. to first evaluate m, we need to pass a continuation to it
--    that takes m's return value as parameter.
-- 2. in the continuation of m, we need to some how pass the continuation c
--    to continue the computation
-- 3. how to use c? we have function k :: (a -> K b) which give us another
--    continuation.
-- 4. a is the return value bind from m. Implies after we evalautes m, it's
--    return value is passed into m's continuation and binds to a.
--    Then we call (k a), which give us a new cont monad.
--    This cont monad takes another continuation to eval. In this case we just
--    pass the c at the very beginning.
-- 5. For CPS, nesting indicates the progress of the computation. The more nested it is
--    the further the computation goes.
bindK :: K r a -> (a -> K r b) -> K r b
(K m) `bindK` k = K
                $ \c -> m (\a -> (cont $ k a) c)

-- capture the current continuation and pass it to the current expression.
-- Why do you need this?
-- first, how do you use this?
callccK :: ((a -> K r b) -> K r a) -> K r a
callccK f = K $ \c -> let k a = K $ \_ -> c a
                       in cont (f k) c

{-@ Continuation interpreter
@-}

{-@ Monad and CPS @-}
