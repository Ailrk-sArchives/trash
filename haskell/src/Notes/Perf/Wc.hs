{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Notes.Perf.Wc where
import System.Environment
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import Foreign
import Foreign.ForeignPtr
import Data.Word

-- different wc with different performance.
standardwc :: IO ()
standardwc = print . length . lines =<< getContents

-- fascter [char]
-- without duplication involved in lenth . lines
fascterwc :: IO ()
fascterwc = interact (count 0)
  where count i []          = show i
        count i ('\n':xs)   = count (i+1) xs
        count i (_:xs)      = count i xs

-- with bytestring
-- it uses packed byte arrys rather than heap allocated [Char] to
-- represent string.
bswc :: IO ()
bswc = B.getContents >>= print . B.count '\n'

-- lazy list of strict, L1-cache-sized chunks of bytes.
lbswc :: IO ()
lbswc = L.getContents >>= print . L.count '\n'

-- ptr hacking
#define STRICT4(f) f a b c d | a `seq` c `seq` d `seq` False = undefined
ptrhacking :: IO ()
ptrhacking = head <$> getArgs >>= B.readFile >>= \(BS.PS x _ l) ->
  withForeignPtr x $ \p -> go p l 0 0
    where
      go :: Ptr Word8 -> Int -> Int -> Int -> IO ()
      STRICT4(go)
      go p l n i
        | n >= l = print i
        | otherwise = do
            (w::Word8) <- peek (p `plusPtr` n)
            go p l (n+1) $ if w == 0x0a  then i+1 else i

run :: IO ()
run = do
  arg <- getArgs
  case arg !! 1 of
    "std" -> standardwc
    "faster" -> fascterwc
    "bs" -> bswc
    "lbs" -> lbswc
    "ptrhack" -> ptrhacking
  putStrLn "End"
