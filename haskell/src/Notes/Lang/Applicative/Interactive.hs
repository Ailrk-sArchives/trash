module Notes.Lang.Applicative.Interactive where

import Text.Read

interactiveDoubling :: IO ()
interactiveDoubling = do
  putStrLn "Choose a number"
  s <- getLine
  let mx = readMaybe s :: Maybe Double
  case mx of
    Just x -> putStrLn ("The double of " ++ show x ++ " is " ++ show (2 * x))
    Nothing -> putStrLn "THis is not a valid number, try again..."
  interactiveDoubling

-- IO as functor
interactiveSumming :: IO ()
interactiveSumming = do
  putStrLn "Choose two numbers:"
  mx <- readMaybe <$> getLine
  my <- readMaybe <$> getLine
  case (+) <$> mx <*> my :: Maybe Double of
     Just z -> putStrLn $ "The sum of your number: " ++ show z
     Nothing -> putStrLn "Invalid number, Retry..."
  interactiveSumming

-- IO as Applicative
interactiveConcatenating :: IO ()
interactiveConcatenating = do
  sz <- putStrLn "Concat two Strings" *> ((++) <$> getLine <*> getLine)
  putStrLn "Result:" *> putStrLn sz

