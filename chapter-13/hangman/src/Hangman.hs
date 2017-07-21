module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String]
                    deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLenght :: Int
minWordLenght = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength w = let l = (length w)
                          in minWordLenght < l && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] GuessedSoFar
                      {-1        2         3-}
{-1 the word we are tring to guess-}
{-2 the characters we've filled n so far-}
{-3 the letters we've guessed so far-}
type FailedAttemps = Int
data GuessedSoFar = GuessedSoFar [Char] FailedAttemps

instance Show GuessedSoFar where
  show (GuessedSoFar chars attemps) =
    chars ++ " (" ++ (show attemps) ++ " failed attemps)"

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ (show guessed)

freshPuzzle :: String -> Puzzle
freshPuzzle w =
  Puzzle w (fmap (const Nothing) w) (GuessedSoFar [] 0)

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) x =
  elem x w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ (GuessedSoFar ag _)) x =
  elem x ag

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar (GuessedSoFar s tries)) c =
  Puzzle word newFilledSoFar newGuessedSoFar
    where
      newFilledSoFar = zipWith (zipper c) word filledInSoFar
      newGuessedSoFar = if newFilledSoFar == filledInSoFar
                           then GuessedSoFar (c : s) (tries + 1)
                           else GuessedSoFar (c : s) tries

      zipper guessed wordChar guessChar =
        if wordChar == guessed
         then Just wordChar
         else guessChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
                \ character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
               \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ (GuessedSoFar chars tries)) =
  if tries > 7 then
                  do putStrLn "You Lose!"
                     putStrLn $ "The word was: " ++ wordToGuess
                     exitSuccess
                  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
  if all isJust filledInSoFar
     then
      do putStrLn $ "You win! The word was: " ++ word
         exitSuccess
     else return ()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must\
                  \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
   in runGame puzzle
