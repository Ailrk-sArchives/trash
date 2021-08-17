{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Cat.ContMonad where

-- What's good
-- 1. `runCont k id` to unwrap a continuation
-- 2. callCC $ \k -> do
--      ...
--      k (..)
--      ...
--    get current continuation. cont can be nested in this way.
-- 3. do n <- (ContT $ \k -> k ...) use continuation without effect.
-- 4. ((a -> r) -> r) -> (a -> r) -> r for suspend computation.
-- 5. cps as ir gives explicit control flow, named intermediate values, and
--    explicit evaluation order.
-- 6. ContT IO r a really feels like an imperative language.

------------------------------------------------------------------
-- Think ConT as a monadic interface of CPS tranformation, which transform
-- direct call into cps form.
-- then runContT is really an interpreter that does a full program transform
-- first then evaluate the transformed code.


import           Control.Monad.Cont
import           Data.Char
import           Debug.Trace

------------------------------------------------------------------
-- cps basics

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

------------------------------------------------------------------
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

------------------------------------------------------------------
-- Monad bind is actually cps transformed

-- here m is computed a value in m is pulled out, then f is feed with that value.
-- this process chain up.
pipeline1 m f g h = m >>= f >>= g >>= h

-- it's clearer if we pull value in m out. the continuation really becomes a
-- function takes x.
pipeline2 m f g h = do
  a <- m
  (\x -> f x >>= g >>= h) a

pipeline3 f g h = callCC (\k -> k 1) >>= f >>= g >>= h
foo3 a = (+a) <$> return 1

-- >>> :set -XScopedTypeVariables
-- >>> let (n1 :: Cont Int Int) = pipeline3 foo3 foo3 foo3
-- >>> runCont n1 id


------------------------------------------------------------------
-- Cont monad for control flow
-- NOTE: Cont being a Monad means effect can't escape it.

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

prod :: Eq a => Num a => [a] -> Cont r a
prod l = callCC $ \k -> loop k l
  where
    loop _ [] = return 1
    loop k (0:_) = k 0  -- if hit an 0 just return 0
    loop k (x:xs) = do
      n <- loop k xs  -- other wise product
      return (x * n)

-- >>> runCont (prod [1, 2, 3, 0, 1]) id
-- >>> runCont (prod [1, 2, 3, 1]) id
-- 0
-- 6

-- well this is not an example
fibonacci :: Eq a => Num a => Int -> a
fibonacci n = loop n (1, 1)
  where
    loop 0 _        = 1
    loop 1 _        = 1
    loop n (f1, f2) = (loop (n - 1) (f2, f1 + f2)) + f1 + f2

-- >>> fibonacci 1
-- >>> fibonacci 2
-- >>> fibonacci 3
-- 1
-- 3
-- 6

-- with contination you can try to do some really imperative looking programmig
binarySearch :: Show a => Eq a => Ord a => Num a => [a] -> a -> Bool
binarySearch xs v = (`runCont` id) . callCC $ \k -> loop k 0 (length xs - 1)
  where
    loop k left right
      | xs !! mid == v = k True
      | left == right = k False
      | otherwise = if xs !! mid > v
                       then (loop k left mid)
                       else (loop k (mid + 1) right)
      where mid = (left + right) `div` 2

-- >>> binarySearch [1..10] 4

-- simulate exception with con
divExcept :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcept x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) $ notOk "Denominator 0"
    ok $ x `div` y
  handler err

-- General exception handling
-- recall monad error
-- class Monad m => MonadError e m | m -> e where
--   throwError :: e -> m a
--   catchError :: m a -> (e -> m a) -> m a
-- tryit is the cps transformed version of it.

tryit :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryit c h = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    x <- c notOk
    ok x
  h err

-- >>> runCont (divExcept 10 2 error) id
-- >>> runCont (divExcept 10 0 error) id
-- 5
-- Denominator 0

------------------------------------------------------------------
-- a and (forall r. (a -> r) -> r)) are isomorpihc
class Iso a b where
  from :: a ->  b
  to :: b -> a

newtype Conti a r = Conti { runConti :: forall r. (a -> r) -> r }

instance Iso a (Conti a r) where
  from a = Conti $ \k -> k a
  to (Conti k) = k id


-- exitential types are elimimated via continuation passing
data Any where
  Any :: a -> Any

-- there exists an a in Any that we know will give us r.
elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

-- >>> elimAny (\a -> 1) (Any 1)
-- 1


------------------------------------------------------------------is a rigid type variable bound by
-- the ContT becaues the effect is
-- scoped. In scheme call/cc capture the rest of the program because call/cc
-- is builtin and global.
--
-- Delimited continuation lets us tag a point in the program so callcc can
-- only capture a portion of the program up to the tag.
--
-- With delimited continuation we can capture arbitrary portions of the program
-- instead of the entire program.


------------------------------------------------------------------
-- cps transformation
-- with callCC we can easily get expose of the current continuation. If
-- we want to use cps as ir, we don't really care about getting the cc,
-- instead we care more about extra information cps provides us. namely
--  explicit control flow
--  explicit evaluaion order
--  named intermediate values.
--  named intermediate values.
