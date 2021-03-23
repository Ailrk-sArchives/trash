module Notes.Lang.Monads.MaybeMonad where

-- source
-- https://wiki.haskell.org/Maybe

not0 :: Int -> Maybe Int
not0 0 = Nothing
not0 x = Just x

not100 :: Int -> Maybe Int
not100 100 = Nothing
not100 x = Just x

h :: Int -> Maybe Int
h x = do n <- not0 x
         not100 n
