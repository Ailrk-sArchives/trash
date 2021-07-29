module Cat.ContMonad where

-- So called mother of all monads.
-- http://blog.sigfpe.com/2008/12/mother-of-all-monads.html


import Control.Monad.Cont

------------------------------------------------------------------
-- A polymorphic monad.
prog1 :: Monad m => m Int
prog1 = do
  a <- return 1
  b <- return 10
  return (a + b)

-- be careful about monomorphization.
runProg1 run = run prog1

runProg1Cont = runProg1 (\prog1 -> runCont prog1 show)
runProg1Maybe = runProg1 (\prog1 -> prog1 :: (Maybe Int))
runProg1List = runProg1 (\prog1 -> prog1 :: [Int])

-- >>> runProg1Cont
-- >>> runProg1Maybe
-- >>> runProg1List
-- "11"
-- Just 11
-- [11]

--  not look at the prog2. we have a hole! The value of ahole will be used
--  in (a + b). Passing ahole direclty is still a direct passing style.
--  How to use continuation to give us access ahole?
-- ------------------
prog2 :: Monad m => Int -> m Int
prog2 b = do
  a <- return 1
  return (a + b)


-- now replace return with it's implementation in ConT monad:
-- the Continuation k gives us control of the surrounding computation.
-- k works similarly as prog2.
prog3 = do
  a <- return 1
  b <- (ContT $ \k -> k 10)
  return (a + b)

-- >>> runCont prog3 show
-- "11"

-- If we choose not to run k at all, the program will come to end at b.
-- The surrounding computation will not run at all.
-- Another way to look at it is we changed the control flow by returning early.
-- This is what we can't do in prog3
prog4 = do
  a <- return 1
  b <- (ContT $ \_ -> "escape")
  return (a + b)

-- >>> runContT prog4 show
-- "escape"

-- We can call the continuation twice.
prog5 = do
  a <- return 1
  b <- (ContT $ \k -> k 10 ++ k 20)
  return (a + b)

-- >>> runContT prog5 show
-- "1121"

-- ------------------
prog5_1 = do
  a <- return 1
  b <- [10, 20]
  return (a + b)

-- >>> prog5_1
-- [11,21]

prog5_2 = do
  a <- return 1
  b <- (ContT $ \s -> concat [s 10, s 20])
  return (a + b)

-- >>> runContT prog5_2  (\x -> [x])
-- [11,21]

-- ------------------
prog5_3 = do
  a <- ["a1", "a2"]
  b <- ["b1", "b2"]
  return (a ++ b)

-- >>> prog5_3
-- ["a1b1","a1b2","a2b1","a2b2"]

-- check the order.
prog5_4 = do
  a <- (ContT $ \k -> k "a1" ++ k "a2")
  b <- (ContT $ \k -> k "b1" ++ k "b2")
  return (a ++ b)

-- >>> runContT prog5_4 show
-- "\"a1b1\"\"a1b2\"\"a2b1\"\"a2b2\""

prog5_5 = do
  a <- return 1
  b <- (ContT $ \k -> [10, 20] >>= k)
  return (a + b)

-- >>> runContT prog5_5 return
-- [11,21]


-- ------------------
-- use ContT to simulate other monad
run :: Monad m => ContT r m r -> m r
run m = runContT m return

-- it's saying we want to apply x to the continuation.
-- The behavior depends on the monad instance.
i :: Monad m => m a -> ContT b m a
i x = ContT $ \k -> x >>= k

v1 = run $ do
  a <- i [1, 2]
  b <- i [10, 20]
  return (a + b)

-- >>> v1

-- Maybe
v2 = run $ do
  a <- i (Just 10)
  b <- i (Just 20)
  return (a + b)

-- >>> v2

-- Conclusion:
--   1. Continuation expose the surrounding computation
--   2. CPS can be used to simulate monad
--   3. Langauges doesn explicitly support monad can support monad with cps.

-- >>> runContT (foo 2) id
-- >>> runContT (foo 20) id
-- "3"
-- "over twenty"

-- or you don't need callCC

pythagoras :: Int -> Int -> ContT m r String
pythagoras a b = do
  a' <- square a
  b' <- square b
  show <$> add a' b'
  where
    add x y = return $ (x + y)
    square x = return $ (x * x)

-- >>> runContT (pythagoras 11 10) id
-- "221"

------------------------------------------------------------------
-- Cont Monad actually hid the control flow.
-- One way to gain back control is to define ContT k explicitly.
-- or we can use callCC to expose the  current continuation

foo :: Int -> ContT m r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return (show $ y - 4)

-- 1. the whole expression is (Int -> ContT m r Int)
-- 2. yet k is also (Int -> ContT m r Int)
-- 3. we wrapped the actually implementation as the paramter to k
-- 4. k is the current continuation
square' :: Int -> ContT m r Int
square' n = callCC $ \k -> k (n * 2)
