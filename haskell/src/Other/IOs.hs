module Other.IOs where

import           Control.Concurrent
import           Control.Monad
import           Data.Functor.Const
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.Random

import           System.IO.Unsafe

-- IO brings explicit effect to haskell.
-- IO turn off many of the ordinary haskell features:
--   1. sharing via non-strict evalution.
--   2. inlining (or not as aggressive)
--   3. laziness


-- nesting to enforece the order
-- to run the lambda, we must evaluate putStr at the left hand side.
-- By deeply nesting we enforce what to be executed before, what after.
printThings :: IO ()
printThings =
  putStr "1" >>= (\_ ->
    putStr "2" >>= (\_ ->
      putStr "3" >>= (\_ -> return ())))

-- make a comparison with continuation
-- we have explicit nesting here. The call back alwasy called
-- after the caller calls.
contForNesting :: Int -> (Int -> r) -> r
contForNesting n k = go n k
  where
    add1 = \n k -> k (n + 1)
    sub1 = \n k -> k (n - 1)
    go n k =
      add1 n (\n1 ->
        add1 n1 (\n2 ->
          sub1 n2 (\n3 -> k n3)))

-- IO implies mutation, it doesn't make sense to share named values any more.

-- We have the sense of time now
-- isn't it fresh? The real world interaction.
realWorldEffect1 :: IO ()
realWorldEffect1 = do
  t1 <- getCurrentTime
  mapM_ (\_ -> getCurrentTime) [1..10000000]
  t2 <- getCurrentTime
  putStrLn (show t1)
  putStrLn (show t2)


-- this creates as many MVar as you want in IO monaad
myData :: IO (MVar Int)
myData = newEmptyMVar

useMyDataDeadLock :: IO ()
useMyDataDeadLock = do
  mv <- myData
  putMVar mv 0

  mv' <- myData    -- this is a different MVar
  zero <- takeMVar mv'
  print zero

useMyData :: IO ()
useMyData = do
  mv <- myData
  putMVar mv 0
  zero <- takeMVar mv
  print zero

useMyDataEvil :: IO () -- this is really forbidden in a normal workflow.
useMyDataEvil = let m = unsafePerformIO newEmptyMVar :: MVar Int
                 in do
                   putMVar m 0
                   zero <- takeMVar m
                   print zero

-- referential transparency on what?
-- By definition it means given a function, same argument alway give
-- the same result.
-- But is it ture even in IO ?
-- You can still perform arbitrary side effects in IO anyway,

-- IO a represents the process of an effectful computation, but it's not
-- tighted to the result they produce anyhow.

-- Given an IO a, you always genrate the same IO action, it's just that the
-- semantics of the IO performs side-effects.

-- takes the example
-- the semantics of gimmeShelter is that:
--  When input is true, generate a list of random value based on the random value
--  generator
--  when input is false, always genrate [0]
-- when argument is true, IO does always produce different values.

{-@ ! @-}
-- no matter what, the IO action always **produce the list of Int the same**
-- instead of exactly the same list of int.
-- That's one of the reason it's warpped in IO.
-- if want the same list all the time, we will ahve type Bool -> [Int]


gimmeShelter :: Bool -> IO [Int]
gimmeShelter True  = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]


-- Functor, Applicative, and Monad of IO

-- notice the effect is in the structure of IO itself, not related to the Int really.
fio = fmap (+1) (randomIO :: IO Int)
aio = (+) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)

-- pure as the effect free embedding of a value in a recipe creating environment.
embedInIO = return :: a -> IO a

performEffectInNestedOrder = join (join (embedInIO (embedInIO (embedInIO "10"))))


{-@
  Nesting lets us express order dependence
  Peter J Ladin (A correspondence between ALGOL 60 and Church's Lambda notation.)
@-}

-- this is not expressible with applicative, as we need need to chose which branch to
-- return based on the result of performing the previous IO effect.
huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True  -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")

resultOfhuehue = either (>>= print) id =<< huehue
