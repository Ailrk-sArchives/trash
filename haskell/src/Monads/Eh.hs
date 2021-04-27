{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Monads.ErrorHandling where

{-@ The initial purpose of this practice is just
    to learn some error handling in haskell, but it
    ended up to be a universal guide for error handling,
    mtl...
    If you don't implemenet mtl once you won't know the
    details.
@-}


import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans


-- Monad transformers are literately a wrapper of any monad
-- over your target type.
-- Today it's ExceptT, tomorrow you can make MaybeT, after
-- you might have IMGuiT, ParserT, ConvertToCPPT or whatever you
-- want it.
newtype ExceptT e m a = ExceptT (m (Either e a))

-- basically runExcept just pull the value after the
-- data constructor out.
-- You wan to work with the underlying monad, ExceptT
-- is just a wrapper to hold all pieces together.
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT m) = m

-- map to an ExceptT means map two layer down into
-- it's inner value.
-- There're actually three layers, the outmost layer is
-- ExceptT itself, but it doesn't carry many useful information.
--
-- One layer inside is the monad wrapper, and that's what
-- you really care about.
-- Because the only constraint for this layer is just being a
-- monad, you can shove in whatever monad you like. Of course,
-- another monad transformer.
-- In this way, you can access different monads all together.
--
-- The innder most is your target type. We have Either here
--
-- fmap a function into a monad transformer means lift over
-- all this three layers.
instance (Monad m) => Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT ((fmap . fmap) f m)

instance (Monad m) => Applicative (ExceptT e m) where
  pure = return
  (<*>)  = ap

-- look at the return you can see three layers clearly.
-- for >>=, be aware after ExceptT we are in the m monad.
instance (Monad m) => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  m >>= k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left e  -> return . Left $ e
      Right a -> runExceptT (k a)

-- define a monadIO so we can bring any IO a over
-- with liftIO.
-- This is big peepee.
instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

-- construct an exceptT with left value.
-- this creates an exception that can be handled easily.
throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

-- carry out inner computation, if an exception throws
-- handle it with the handler
catchE :: (Monad m)
       => ExceptT e m a
       -> (e -> ExceptT e' m a)
       -> ExceptT e' m a

m `catchE` h = ExceptT $ do
  a <- runExceptT m
  case a of
    Left l  ->  runExceptT (h l)
    -- of course if it's right you don't care about e' anymore.
    Right r -> return (Right r)


-- MonadTrans for ExceptT
-- What does lift do is essentially lift a monadic
-- value into the ExcepT
-- There are two things you need to ensure:
--  1. the result is a ExceptT.
--  2. the inner value `a` of `m a` should be lifted to `Either e a`
--  It looks lie this.
--  monad a -> ExceptT | monad | Either e a
instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right


-- This is mtl style monad transformer.
-- What does it do?
-- Essentially it helps you to lift the underlying
-- monad up no matter how many layers it is buried in.
--
-- m can be any monad, as long as we define this
-- typeclass for several monads. If we combine this
-- monads together we can deverive a working MonadError
-- which brings error up even if it is arbitrarily
-- deep in the monad stack.

{-@ Similar to MonadState, MonadError, and MonadIO
    The idea behind an mtl interface is that you
    only use interface they provide, and will never have
    the need to call lift.

    What is mtl really for?
    A mtl class Monad... is basically an interface over any
    other monad.  If a monad has this instance, it can be used
    as the interface stated.

    For most monads, there are only a handful number of opertaions
    that are required. For example, a state monad will allow you
    to get set modify, reader monad will be ask.

    But if multiple monads are wrapped together, the type you access
    will no longer be State monad or Reader monad, instead, you are
    using a  monad transformer stack with State monad, reader monad's
    functionality embeded.

    Normally to call an action from a monad nested in a transformer stack,
    you call lift . lift  until reach the layer. But with mtl you can
    derive an instance of a mtl typeclass, and use it as if the transformer
    stack implemented the type.

    To achieve this we are heavily dependent on generalized newypte deriving,
    since all instances are derive from some monad somewhere in the stack.

    Note, for error handling, MonadError provides an universal interface for
    all errorish type. Either, ExceptT... etc. They are fundamentally different
    types, and you need to provide difference implementation for each of them.

    Another example is monadIO. IO is a huge monad, and it has a plantora of
    action and will be extended by users. It's unlikely to provide a mtl interface
    for all IO actions. That's why MonadIO provides you with liftIO, so instead of
    call putStrLn, you call liftIO putStrLn.
  @-}
class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- without this instance you need to call the
-- specific ioError and catch for IO exceptions. WIth
-- MonadError this problem is handled universally.
instance MonadError IOException IO where
  throwError = ioError
  catchError = catch

instance MonadError e (Either e) where
  throwError = Left
  Left l `catchError` h  = h l
  Right r `catchError` _ = Right r

instance (Monad m) => MonadError e (ExceptT e m) where
  throwError = throwE
  catchError = catchE

{-@ Possible use case @-}

-- throw in the code (If there is an error wrap it in Left.)
throwMeEither:: Int -> Either String Int
throwMeEither n |  n > 100 = Right n
  | otherwise = Left "Bad bad"

throwMeExcept :: Int -> ExceptT String IO Int
throwMeExcept n | n > 100 = return n
  | otherwise = throwError "Bad bad except"

-- capture with an handler. With monad error we can't capture
-- Either and ExceptT with the same function.
catchMe :: MonadError e m => Int -> (Int -> m Int) -> m Int
catchMe n f = catchError (f n) (\_ -> return 1)

{-@ Some exampels @-}
-- define your own exception
data LengthError = EmptyString
                 | StringTooLong Int
                 | OtherError String

instance Show LengthError where
  show EmptyString = "the string is empty"
  show (StringTooLong len) = "The length of the string " ++ (show len) ++ " is too long"
  show (OtherError msg) = msg

newtype Length a = Length { unLengh :: ExceptT LengthError IO a }
  deriving (Functor, Applicative, Monad, MonadError LengthError, MonadIO)

-- note it's not necessary to catch every throw, since throw is really
-- just a pure for the error case anyway.
stringLengthExample  :: IO ()
stringLengthExample = do
  r <- runExceptT (unLengh calculateLength)
  report r

report :: Either LengthError Int -> IO ()
report (Right len) = putStrLn ("length of the str is: " <> show len)
report (Left e)    = putStrLn ("Error: " ++ show e)

calculateLength :: Length Int
calculateLength = do
  liftIO . putStrLn $ "Please enter a string"
  s <- liftIO getLine
  if null s
    then throwError EmptyString
    else let len = length s
          in if len > 50
                then throwError $ StringTooLong len
                else return len

-- catchMe 10 (\n -> if n == 10 then throwError "bad bad" else return 1)

{-@ quirks when catching IO @-}

-- this will crash.
-- First the thunk head [] is created, then it will live for the entire time until you
-- evaluate it, which in this case is never. Essentially no exception happend in the code,
-- the exception happend when ghci try to print the value out.
catchWithReturn = catch (return $ head []) $ \(_ :: SomeException) -> return "good"

-- This wont crash, because ghci doesn't need to print anything, the exceptional code (head [])
-- is still there like a bomb.
catchWithReturnUnit = catch (return $ head []) $ \(_ :: SomeException) -> return ()

-- evaluate force it's argument to be evaluated into weak head normal form.
-- this means (head []) get evaluated in place, and it get returned directly.
catchWithEvaluate = catch (evaluate $ head []) $ \(_ :: SomeException) -> return "good"
