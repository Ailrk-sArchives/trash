module RegisteredUser1 where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

-- RegisteredUser :: Username -> AccountNumber -> User
-- RegisteredUser is a function that constructs a User out of two args.
-- So it is called as data constructor
printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber accNum))
           = putStrLn $ name ++ " " ++ show accNum



