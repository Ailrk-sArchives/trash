module Cat.Cat.C1 where


import           Data.Void

-- can't give an instance of void, this function will never be called.
-- from falsity follows anything
-- f -> t
absurd' :: Void -> a
absurd' a = undefined

f44 :: () -> Integer
f44 () = 44

-- function return void really means it's for side effect
-- so in haskell the equivalent is IO ()
-- function reuturn purely () doesn't do anything special.

funit :: () -> ()  -- utterly useless
funit () = ()

fsideeffect :: () -> IO ()
fsideeffect () = do
  putStrLn "hi"

-- a commuative diagram with only Void, (), and Bool
--
--    // just represent with graph, since it's hard to draw with ascii
--               Void -> ()
--               Void -> Bool
--               Void -> Void
--
--               () -> Bool
--               () -> ()
--
--               Bool -> () 1
--               Bool -> () 2
--               Bool -> Bool
