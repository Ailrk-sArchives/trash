module Main where

import           HttpClientX
import           HsLuaX
import           LuaCallback
import qualified Foreign.Lua                   as Lua

main :: IO ()
main = do
  --httpclientRun
  print "-----------------------------------"
  print "runLuaScript"
  runLuaScript
  print "runCallLuaFromHs"
  runCallLuaFromHs
  print "runCallHsFromLua"
  runCallHsFromLua
  print "lua callbacks"
  luaCallbackRun

  print "-----------------------------------"

