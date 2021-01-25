{-# LANGUAGE ForeignFunctionInterface #-}
module Other.FFI where

import           Foreign
import           Foreign.C.Types
import           Foreign.Ptr
import           System.IO.Unsafe


{-@ Learning to use some ffi
    Marsharling: Converting hasekll types to C types.
@-}

-- use CDouble to represent C types.
-- there is a safety level marker,
-- unsafe is faster then safe, and safe is only required when
-- there are c functions that needto call back haskell function.
--
-- One note: the function signature must be correct!
-- GHC has no idea what type does the underying c fucntion  has.
foreign import ccall unsafe "math.h sin"
  c_sin :: CDouble -> CDouble

hsin :: Double -> Double
hsin = realToFrac . c_sin . realToFrac


-- for impure functions
-- Note for functions with side effects we need to put it into IO.
-- Otherwise haskell will treat it as a pure function and return the
-- same result everytime you call it.
foreign import ccall unsafe "stdlib.h rand"
  c_rand :: IO CUInt

foreign import ccall unsafe "stdlib.h srand"
  c_srand :: CUInt -> IO ()

hsrand :: Int -> IO ()
hsrand =  c_srand . fromIntegral

foreign import ccall unsafe "math.h hypot"
  c_hypot :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "stdlib.h random"
  c_random :: IO CLong

hrandom :: IO Integer
hrandom = fromIntegral <$> c_random

hhypot :: Double -> Double -> Double
hhypot a b = realToFrac $ c_hypot (realToFrac a) (realToFrac b)

-- these are just simple functions that are really not necessary
-- to be imported. Let's try some more complicated ones.


{-@ Working with C pointers @-}

-- any funciton with side effect needs to be wrapped in IO.
foreign import ccall unsafe "gsl/gsl_math.h gsl_frexp"
  gsl_frexp :: CDouble -> Ptr CInt -> IO CDouble

-- ok now we have this IO stuff, but how do we get value in the pointer
-- out yet still provide a pure interface?
frexp :: Double -> (Double, Int)
frexp x = unsafePerformIO $
  alloca $ \expptr -> do
    f <- gsl_frexp (realToFrac x) expptr
    e <- peek expptr
    return (realToFrac f, fromIntegral e)
