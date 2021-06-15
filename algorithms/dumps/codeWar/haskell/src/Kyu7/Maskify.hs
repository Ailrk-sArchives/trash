-- <Credit Card Mask>
module Kyu7.Maskify (maskify) where

-----------------------------------------------
-- first version
-----------------------------------------------

maskify :: String -> String
maskify str =
  let l = length str - 4
   in fmap (const '#') (take l str) ++ drop l str

-----------------------------------------------
-- better version
-----------------------------------------------
-- Same idea, but with replicate
-- replicate repeat const second argument n times
-- into [].
maskify' :: String -> String
maskify' str = replicate l '#' ++ drop l str
  where l = length str - 4
