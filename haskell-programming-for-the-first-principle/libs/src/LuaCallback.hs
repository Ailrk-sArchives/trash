{-# LANGUAGE OverloadedStrings #-}
module LuaCallback where

import Data.IORef
import qualified Data.ByteString.Char8 as BC
import Control.Monad (void)
import Foreign.Lua

luaCallbackRun :: IO ()
luaCallbackRun = do
  callback <- newIORef []  -- create IORef. mutable values in IO monad.
  run $ do
    openlibs
    registerHaskellFunction "addLuaCallbacks" (addLuaCallbacks callback)
    registerHaskellFunction "callLuaCallbacks" (callLuaCallbacks callback)
    registerHaskellFunction "resetLuaCallbacks" (resetLuaCallbacks callback)
    void $ dofile "lua/callback.lua"

type LuaFunRef = Reference

-- put addCallbacks into ref list.
addLuaCallbacks :: IORef [LuaFunRef] -> Lua NumResults
addLuaCallbacks cs = do
  -- get the topmost index of argument
  args <- gettop
  as <- checkArgs args
  case as of
    Nothing -> do  -- no problem
      addCallbacks 1 args
      return 0
    Just errArg -> do
      pushstring $ BC.pack $  -- push string to the top
        "argument " ++ show errArg ++ " is not a function"
      return 1

    where
      -- check if all args are functions
      checkArgs :: StackIndex -> Lua (Maybe StackIndex)
      checkArgs 0 = return Nothing
      checkArgs n = do
        ty <- ltype n
        if ty == TypeFunction
           then checkArgs (n - 1)
           else return $ Just n

      -- arg is the range of index of arguments.
      addCallbacks :: StackIndex -> StackIndex -> Lua ()
      addCallbacks n maxIdx
        | n > maxIdx = return ()
        | otherwise = do
            pushvalue n
            refId <- ref registryindex
            liftIO $ modifyIORef cs (++ [refId])
            addCallbacks (n+1) maxIdx

callLuaCallbacks :: IORef [LuaFunRef] -> Lua NumResults
callLuaCallbacks cs = do
  cs' <- liftIO $ readIORef cs
  createtable (length cs') 0
  iter cs'
  return 1
    where
      iter [] = return ()
      iter (c:rest) = do
        getglobal' "table.insert"
        pushvalue (-2)
        getref registryindex c
        call 0 1
        call 2 0
        iter rest

resetLuaCallbacks :: IORef [LuaFunRef] -> Lua NumResults
resetLuaCallbacks cs = do
  cs' <- liftIO $ readIORef cs
  mapM_ (unref registryindex) cs'
  liftIO $ writeIORef cs []
  return 0

