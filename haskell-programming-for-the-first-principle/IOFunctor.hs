module IOFunctor where

getInt :: IO Int
getInt = read <$> getLine

-- functor IO
meFunctor :: IO String
meFunctor = (++ "Me too! I am Functor") <$> getLine

-- monad IO
meTooIsm :: IO String
meTooIsm = do
    input <- getLine
    return (input ++ "and me too! I am monad")

