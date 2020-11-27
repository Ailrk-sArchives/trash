{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MTLPractise where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer


-- THis nice example combined read, write, and state monads all
-- together.

-- You use Reader monad when you want to read something from the
-- environment without needing to passing the environment around.

-- You use writer monad when you want to write to some value without
-- explicity passing where to write.

-- You use state monad when you have some state needs to be modified
-- but you don't want to explicitly pass then around.

-- So, see, all this concepts are super simple and the use case
-- are quite clear. By using these monads and compose them as a
-- huge monad transformer, you precisely defined a world that
-- where you can read from, where you can write to, and what
-- kind of stuffs can be changed.


-- This is a simple virtual machine.
-- Read from Program, Act on the Stack, and output to Output.
data Instr = Push Int | Pop | Puts

type Stack = [Int]
type Output = [Int]
type Program = [Instr]

-- First let's build our burrito.
--
type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a  = Comp { unComp :: VM a}
  deriving newtype (Functor, Applicative, Monad, MonadState Stack, MonadReader Program, MonadWriter Output)


evalInstr :: Instr -> Comp ()
evalInstr instr = case instr of
                    Pop -> modify tail
                    Push n -> modify (n:)
                    Puts -> do
                      tos <- gets head
                      tell [tos]

eval :: Comp ()
eval = do
  instr <- ask
  case instr of
    []     -> return ()
    (i:is) -> do
      evalInstr i
      local (const is) eval


-- This is mysterous until you know what each function does.
-- First of all, all monad transformer are associated with
-- a corresponding "executer", the main purpose is to get
-- the underlying value out of the data constructor.
--
-- For instance, runReaderT get (r -> m a) out so you can use it.
--
-- Second, you might want many different types of runners on the
-- same transformer, so you can get different aspect of it.
--
-- For instance, you have evalState and execState.
-- evalState gives you the value, while execState gives you the
-- state.
--
-- And for Reader, you only have runReader, which gives you
-- the environment.
execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval)
