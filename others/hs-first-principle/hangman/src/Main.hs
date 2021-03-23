module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


type WordList = [String]

minWordLen = 5 :: Int
maxWordLen = 9 :: Int

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)


gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLen aw)
    where gameLen w =
            let l = length (w :: String)
             in l > minWordLen && l < maxWordLen

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered gussed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Gussed so far: " ++ gussed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem c w

alreadyGussed :: Puzzle -> Char -> Bool
alreadyGussed (Puzzle _ _ gussed) c = elem c gussed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c:s)
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed
               then Just wordChar
            else guessChar

        newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

handleGUess :: Puzzle -> Char -> IO Puzzle
handleGUess puzzle guess = do
    putStrLn $ "Your guess was:" ++ [guess]
    case (charInWord puzzle guess , alreadyGussed puzzle guess) of
      (_, True) -> do
          putStrLn "You already guessed that\
                    \ character, pick something else!"
          return puzzle
      (True, _) -> do
          putStrLn "This character was in the word,\
                   \ filling in the word accordingly."
          return (fillInCharacter puzzle guess)
      (False, _) -> do
          putStrLn "This character wasn't in the word, try again."
          return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 20
       then do putStrLn "You lose!"
               putStrLn $ "The word was: " ++ wordToGuess
               exitSuccess
       else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
    if all isJust filledInSoFar
       then do putStrLn $ "You win! " ++ wordToGuess
               exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGUess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle



