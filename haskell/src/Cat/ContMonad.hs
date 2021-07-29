module Cat.ContMonad where

-- So called mother of all monads.
-- http://blog.sigfpe.com/2008/12/mother-of-all-monads.html

-- CPS transform:
-- We have value x :: a, in direct passing style to use it we just use it.
-- e.g (foo :: a -> b) (x)
-- To cps transform the program, we need to make x a suspend operation
-- x' :: ((a -> r) -> r). The value x is passed (or think it as returned) to
-- the continuation passed in.
--
-- 1. all program can be cps transformed
-- 2. some implicit properties are exposed explicitly in cps
--    - intermediate values are all named
--    - procedure return is explicit
--    - tail call becomes calling the continuation


import Data.Char
import Control.Monad.Cont

------------------------------------------------------------------
-- cps basics

---------------------
-- direct style
-- NOTE:
--   1. the return is implicit. we return when the entire expression is evaluated
--   2. the order of evaluation of arguments is implicit. It depends on
--      implementation
--   3. the intermediate value x * x, y * y, (x * x) + (y * y) is implicit,
--      they don't have a name and we can't refer to them.
pyth1 x y = sqrt ((x * x) + (y * y))

-- cps transformed
-- NOTE:
--  1. the return is explicit. It returns we call k.
--     k is the continuation of the entire program, and once it's called the
--     current call stack can be elimiated.
--  2. the order of evaluation is explicit. x is evaluated before y due to
--     the order of nesting
--  3. intermediate values are explicit. Note now we have control to x2, y2,
--     x2y2, which are all implicit in direct passing style.
pythcps x y k =
  multcps x x $ \x2 ->
  multcps y y $ \y2 ->
  addcps x2 y2 $ \x2y2 ->
  sqrtcps x2y2 k
  where
    multcps x y k = k (x * y)
    addcps x y k = k (x + y)
    sqrtcps x k = k (sqrt x)

-- compositions
type Suspend a r = ((a -> r) -> r)
type Continuation a r = a -> r
type MonadicOperatio m a b = m a -> (a -> m b)
type Pipeline a b = a -> b
-- it's very similar with monad, the control is passed to the next function
-- (actually monad)

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

-- >>> runContT (foo 2) id
-- >>> runContT (foo 20) id
-- "3"
-- "over twenty"

-- 1. the whole expression is (Int -> ContT m r Int)
-- 2. yet k is also (Int -> ContT m r Int)
-- 3. we wrapped the actually implementation as the paramter to k
-- 4. k is the current continuation
square' :: Int -> ContT m r Int
square' n = callCC $ \k -> k (n * 2)

-- Complex control flow
-- callCC really is just a convinent way to say (ConT k)

fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \exit1 -> do
    when (n < 10) $ exit1 (show n)
    let ns = fmap digitToInt (show (n `div` 2))
    n' <- callCC $ \exit2 -> do
      when ((length ns) < 3) (exit2 (length ns))
      when ((length ns) < 5) (exit2 n)
      when ((length ns) < 7) $ do
        let ns' = map intToDigit (reverse ns)
        exit1 (dropWhile (=='0') ns')
      return $ sum ns
    return $ "ns = " ++ (show ns) ++ ") " ++ (show n')
  return $ "Answer: " ++ str

-- We no longer in monad with ConT alone.
-- callCC gives us nested continuation
fun' :: Int -> String
fun' n = (`runContT` id) $ do
  str <- ContT $ \k1 ->
    if (n < 10)
       then k1 "end"
       else "not end"
  return str


