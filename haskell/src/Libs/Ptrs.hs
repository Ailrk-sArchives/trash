module Libs.Ptrs where

import           Control.Exception     (SomeException, try)
import           Control.Monad         (forM_)
import           Data.Char             (chr)
import           Data.Word             (Word8)
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        newForeignPtr_, withForeignPtr)
import           Foreign.Marshal.Alloc (free, malloc)
import           Foreign.Ptr           (FunPtr, Ptr, plusPtr)
import           Foreign.Storable      (alignment, peek, poke, sizeOf)
import           System.IO.MMap        (Mode (..), mmapFileForeignPtr)

{-@ Ptr a
--  - Represent a pointer to an obj or an array of obj
--  - Type a usually can be marshalled from and to haskell object.
--  - a usually will have an instance for Storable
--  - Storable provides marshalling operations.
--  - Note Storable is not necessary, you can
--    write your own implementation for marshalling.
@-}

{-@ ForeignPtr a
--  - similar to Ptr
--  - reference to object that is not managed by haskell rts
--  - associated with a finalizer
--  - finalizer is invoked when the ForeignPtr is gc'ed
--  - usually finalizer calls a routine in foreign lang to free rescource.
@-}

-- Note with Ptr alone you can only play with the address, but can't do
-- anything with values.
-- Storable is necessary because you need that to manipulate data in haskell
-- world.

-- FunPtr a
--  - A ptr to a function callable from foreign code.
--    e.g pfree :: FunPtr (Ptr a -> IO ())

foreign import ccall "stdlib.h &free"
  pfree :: FunPtr (Ptr a -> IO ())

-- this can be an example of finalizer from other language.
--
-- Example of using pfree as finalizer for a foreign ptr
-- This function takes a heap allocated integer, read it's
-- value, add 10 to it and store it back.
-- Finally when the function is finished free the mem of the integer.

readStuff :: Ptr Word8 -> IO ()
readStuff p = do
  val <- peek p               -- Storable peek.
  let align = alignment val
  let sz = sizeOf val
  putStrLn $ "alignment for int: " <> show align
  putStrLn $ "size of int: " <> show sz
  -- there are also peekEleOff, peekByteOff etc for c ptr arrays.
  poke p $ 10 + val           -- Storable poke.


-- allocate stuffs

allocateStuff :: IO ()
allocateStuff = do
  p <- malloc :: IO (Ptr Word8)
  value <- peek p
  putStrLn $ "newly malloced word8, the value is " ++ show value
  poke p $ value + 128

  newval <- peek p
  putStrLn $ "new value is of malloced word8 is " ++ show newval
  free p


doStuff :: ForeignPtr Word8 -> Int -> IO ()
doStuff fp i =
  withForeignPtr fp $ \p -> do
    let addr = p `plusPtr` i
    val <- peek addr :: IO Word8
    print (addr, val, chr $ fromIntegral val)
    readStuff p
    return ()


run :: IO ()
run = do
  (p, offset, size) <- mmapFileForeignPtr path mode range
  forM_ [0 .. size - 1] $ \i -> do
    doStuff p (offset + i)
  mapM_(\_ -> do allocateStuff) [0..10]
  where
    path = "/tmp/input.dat"
    mode = ReadWrite
    range = Nothing
