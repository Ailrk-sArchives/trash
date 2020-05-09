module Practice where

import Control.Monad.Trans.State

-- 1.
get' :: State s s
get' = state $ \s -> (s, s)

--2.
put' :: s -> State s ()
put' s = state $ (,) ()

--3.
exec' :: State s a -> s -> s
exec' sa s = snd $ (runState sa) s

--4.
eval' :: State s a -> s -> a
eval' sa s = fst $ (runState sa) s

--5.
modify'' :: (s -> s) -> State s ()
modify'' ss = state $ \s -> ((), ss s)







