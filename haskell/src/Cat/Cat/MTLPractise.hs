{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Cat.Cat.MTLPractise where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bits
import           Data.Int

-- An example that combined read, write, and state monads all
-- together.

-- 1. Reader monad when you want to read something from the
-- environment without needing to passing the environment around.
--
-- 2. Writer monad when you want to write to some value without
-- explicity passing where to write.
--
-- 3. State monad when you have some state needs to be modified
-- but you don't want to explicitly pass then around.
-- What if I want them all!?

-- This is a simple virtual machine.
-- Read from Program, Act on the Stack, and output to Output.
data Instr = Push Int | Pop | Puts | Add | Sub | Mul | Div | ShiftL | ShiftR
type Stack = [Int]
type Output = [Int]
type Program = [Instr]

-- build up the transformer stack.
-- it's really ugly to be honest.
type VM a = ReaderT Program (
            WriterT Output (
            State Stack)) a

newtype Comp a = Comp {unComp :: VM a}
  deriving newtype (Functor, Applicative, Monad, MonadState Stack, MonadReader Program, MonadWriter Output)

evalInstr :: Instr -> Comp ()
evalInstr = \case
  Pop -> modify tail
  Push n -> modify (n :)
  Puts -> do
    tos' <- get
    case tos' of
      [] -> tell [-1]
      _  -> tell [head tos']
  Add -> binary (+)
  Sub -> binary (-)
  Mul -> binary (*)
  Div -> binary div
  ShiftR -> binary shiftR
  ShiftL -> binary shiftL
  where
    binary apply = do
      a1 <- gets head
      evalInstr Pop
      a2 <- gets head
      evalInstr Pop
      evalInstr $ Push (apply a2 a1)

eval :: Comp ()
eval = do
  instr <- ask
  case instr of
    [] -> return ()
    (i : is) -> do
      evalInstr i
      local (const is) eval

-- again, the idiom for monad transformers is to wrap them in newtypes, so
-- you can derive stuffs, have oquaue abstractions, etc.
-- But this means if you want to run a stack, you need to unwrap it's layers
-- one by one.
--
-- We are writing functional language, the entire program is a giant function.
-- and here is really the entrance. We provide the initial states, environment
-- and everything else that the program might need.
-- Then it just unwrap all layers in the monad transformer stack,
-- which perform their own effects.

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval)

program :: Program
program =
  [ Push 47,
    Push 27,
    Add,
    Puts,
    Push 2,
    Push 3,
    ShiftL,
    Puts
  ]

run :: IO ()
run = mapM_ print $ execVM program
