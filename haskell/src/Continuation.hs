{-# LANGUAGE DeriveFunctor              #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- The point of continuation is to alter the control flow of a program.
-- You can early return by call the continuation in the middle of the program.
-- You also can use continuation to handle exceptions and failures by passing
-- a continuation for success case, and another for failure case.
--
-- Parsec interally use continuation heavily. ParsecT has 4 callbacks, each
-- one will be invoked at certain circustance.
--
-- With continuation, you have the next computation at hand, and you can
-- make the decision where to continue the computation.

module Continuation where

import           Control.Monad
import           Control.Monad.Cont
import           Data.Char
import           Data.IORef
import           Foreign.C.String      (CString, withCString)
import           Foreign.Marshal.Array (withArray0)
import           Foreign.Ptr

-- direct style
id' :: a -> a
id' a = a

{-@ continuation @-}
-- continuation passing style
idCPS :: a -> (a -> r) -> r
idCPS a cont = cont a

-- the big idea is instead of return a value,
-- pass the continuation of the execution as a parameter
-- of the function, and feed the result as an argument of the
-- continuation.

-- consider ($) :: (a -> b) -> a -> b
-- ($ 2) makes it as if we are appling a value to a function.
demo1 = map ($ 2) [(2 *), (3 *), (4 +)]

{-@ suspend computation @-}
-- look at  this type
-- f :: (a -> r) -> r
-- f is a suspend computation.
-- to complete it, we need to pass a
-- funtion with type (a -> r), which represents the continuation
-- of the current computation.

-- this function converts a value to a suspension.
newtype Suspend r a = Suspend { unSuspend :: (a -> r) -> r }
mkSuspend = Suspend . flip ($)

-- some simple examples

mysqrt :: Floating a => a -> a
mysqrt = sqrt

mysqrtCPS :: Floating a => a -> (a -> r) -> r
mysqrtCPS a k = k (sqrt a)

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * (fact $ n - 1)

factCPS :: Integral a => a -> (a -> r) -> r
factCPS 0 k = k 1
factCPS n k = k (n * (fact $ n - 1) )

main :: IO ()
main = putStrLn . show $ factCPS 10 (\a -> mysqrtCPS (fromIntegral a) id)

-- intermediate structures
multiCont :: [(r -> a) -> a] -> ([r] -> a) -> a
multiCont xs = runCont (mapM cont xs)
withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act =
  multiCont (map withCString strings) (\rs -> withArray0 nullPtr rs act)

-- write some functions in continuation passing style.
pythagorasCPS :: Int -> Int -> Cont r Int
pythagorasCPS x y = do
  xsquared <- pure $ (x ^ 2)
  ysqyared <- pure $ (y ^ 2)
  s <- add_cont xsquared ysqyared
  fmap floor (sqrt_cont $ fromIntegral s)
  where
    add_cont :: Int -> Int -> Cont r Int
    add_cont x y = return ((+) x y)

    sqrt_cont :: Floating a => a -> Cont r a
    sqrt_cont x = return (sqrt x)

gcdCPS :: Int -> Int -> Cont r Int
gcdCPS x y = if y == 0 then return x else do
  r <- rem_cont x y
  gcdCPS y r
  where
    rem_cont :: Int -> Int -> Cont r Int
    rem_cont x y = return (rem x y)

lcmCPS :: Int -> Int -> Cont r Int
lcmCPS x y  = do
  mul_cont x y >>= \m -> abs_cont m >>= \a -> do
    gcd <- gcdCPS x y
    div_cont a gcd
  where
    abs_cont :: Int -> Cont r Int
    abs_cont x = pure ((\x -> if x >= 0 then x else negate x) x)
    div_cont :: Int -> Int -> Cont r Int
    div_cont x y = pure (div x y)
    mul_cont :: Int -> Int -> Cont r Int
    mul_cont x y = pure (x * y)

-- write it without make everthing cps.
lcmCPS' :: Int -> Int -> Cont r Int
lcmCPS' x y = do
  m <- pure $ (x * y)
  a <- pure $ (abs m)
  gcd <- gcdCPS x y
  return $ (a `div` gcd)

{-@ continuation monad @-}
-- let's define our own continuation monad.
newtype Cont' r a = Cont' { runCont' :: (a -> r) -> r } deriving Functor

instance Applicative (Cont' r) where
  pure = return
  (<*>) = ap

instance Monad (Cont' r) where
  return = Cont' . flip ($)
  (Cont' c) >>= f = Cont' $ \k -> c (\a -> runCont' (f a) k)

{-@ callCC @-}
-- continuation monad allows us to write continuation with
-- normal order. But with monadic facilities along, we can't
-- exploit the full power of continuation: arbitrarily control the
-- flow control.
--
-- callCC helps us to do that.

squareCPC :: Int -> Cont r Int -- without callcc
squareCPC n = return (n ^ 2)

squareCC :: Int -> Cont r Int -- with callcc. Expose the current continuation out.
squareCC n = callCC $ \k -> k (n * 2)

-- callcc calls the current continuation as its argument.
class Monad m => MonadCont' m where
  callCC' :: ((a -> m b) -> m a) -> m a

instance MonadCont' (Cont' r) where
  callCC' f = Cont' $ \k -> runCont' (f (\a -> Cont' $ \_ -> k a)) k
-- how is continuation be used?
fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \exit1 -> do            -- define exit1
    when (n < 10) (exit1 (show n))
    let ns = map digitToInt (show (n `div` 2))
    n' <- callCC $ \exit2 -> do           -- define exit2
      when ((length ns) < 3) (exit2 (length ns))
      when ((length ns) < 5) (exit2 n)
      when ((length ns) < 7) $
        do let ns' = map intToDigit (reverse ns)
           exit1 (dropWhile (=='0') ns')   -- escape 2 levels
      return $ sum ns

    return $ "(ns = " ++ (show ns) ++ ")" ++ (show n')
  return $ "Answer: " ++ str


{-@ continuation monad and callCC @-}

-- use callCC allows you to decide when use k
-- early return
foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return (show $ y - 4)


bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c:s
    when (s0 == "Hello") $ k "They say hello"
    return ("They appear to be saying " ++ s0)
  return $ length msg

quux :: Cont r Int
quux = callCC $ \k -> do
  let n = 5
  k n
  return 25 -- this wil never be called

-- use callCC as a goto.
getCC :: MonadCont m => m (m a)
getCC = callCC (\k -> let x = k x in return x)

-- PS: no matter what, try traverse is always right.
loopyWithcallCC :: IO ()
loopyWithcallCC = do
  c <- liftIO $ newIORef (10 :: Int)
  flip runContT return $ do
    k <- getCC  -- goto
    traverse liftIO [ (putStrLn "Hello1"), modifyIORef c $ (\x -> x-1) ]
    liftIO $ (putStrLn "Hello2") >> (modifyIORef c $ (\x -> x-1))
    c' <- liftIO . readIORef $ c
    when (c' > 0) k
    liftIO . putStrLn $ "counter is zero now"
  putStrLn "Done"

{-@ Continuation for exception @-}

-- implemented with two continuations
divExcept :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcept x y handler = callCC $ \ok -> do
  err <- callCC $ \notok -> do
    when (y == 0) $ notok "Denominator 0"
    ok $ x `div` y
  handler err

indexExcept :: [a] -> Int -> (String -> Cont r a) -> Cont r a
indexExcept xs idx handler = callCC $ \ok -> do
  err <- callCC $ \notok -> do
    when (length xs - 1 < idx) $ notok "out of bound"
    ok $ xs !! idx
  handler err

-- more general exception handling
tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryCont c h = callCC $ \ok -> do
  err <- callCC $ \notok -> do
    x <- c notok
    ok x
  h err

data SqrtException = LessThanZero deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
  ln <- lift (putStrLn "Enter a number to sqrt: " >> readLn)
  when (ln < 0) (throw LessThanZero)
  lift $ print (sqrt ln)

runSqrtIO :: IO ()
runSqrtIO = runContT (tryCont sqrtIO (liftIO . print)) return
