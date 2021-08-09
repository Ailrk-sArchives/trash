{-# LANGUAGE OverloadedStrings #-}
module HsLuaX where

import qualified Foreign.Lua as Lua
import Data.ByteString.Char8
import qualified Data.ByteString as B

runLuaScript :: IO ()
runLuaScript = Lua.run $ do
  Lua.openlibs
  Lua.dofile "lua/configfile.lua"
  name <- Lua.getglobal "username" *> Lua.peek (-1)
  pwd <- Lua.getglobal "password" *> Lua.peek (-1)
  Lua.liftIO $ print (name :: String, pwd :: String)

runCallLuaFromHs :: IO ()
runCallLuaFromHs = Lua.run $ do
  Lua.openlibs
  Lua.dofile "lua/configfile.lua"
  (name, pwd) <- Lua.callFunc "getuserpwd" ("mail.google.com" :: String)
  Lua.liftIO $ print (name :: String, pwd :: String)

hsMove :: Lua.Integer -> String -> Lua.Lua String
hsMove (Lua.Integer n) direction = return
  ("going " ++ show n ++ " step(s) " ++ direction)

runCallHsFromLua = Lua.run $ do
  Lua.openlibs
  Lua.registerHaskellFunction "move" hsMove
  Lua.loadfile "lua/game.lua"
  Lua.call 0 0


