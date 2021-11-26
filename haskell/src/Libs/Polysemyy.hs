{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Libs.Polysemyy where

-- polysemy is based on free monad, we define a data type for operations and
-- embed effects into it.
-- The effect is performed by a separate interpreter

import Polysemy
import Polysemy.Input
import Polysemy.Output

data TeleType m a where
  ReadTTY :: TeleType m String
  WriteTTY :: String -> TeleType m ()

makeSem  ''TeleType

teletypeToIO :: Member (Embed IO) r => Sem (TeleType ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeleTypePure :: [String] -> Sem (TeleType ': r) a -> Sem r ([String], a)
runTeleTypePure i = runOutputMonoid pure . runInputList i . reinterpret2 \case
  ReadTTY -> maybe "" id <$> input
  WriteTTY msg -> output msg

echo :: Member TeleType r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo


echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeleTypePure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

run1 :: IO ()
run1 = runM . teletypeToIO $ echo

