module Excersizes where

import Data.Maybe

vowels = "aeiou"

{-ex1-}
replaceAllThe :: String -> Maybe String
replaceAllThe [] = Nothing
replaceAllThe text = Just (unwords $ (map replaceThe) $ words text)

replaceThe :: String -> String
replaceThe "the" = "a"
replaceThe x = x

{-ex2-}
countTheBeforeVowel :: String -> Int
countTheBeforeVowel text = myCount $ words text

myCount :: [String] -> Int
myCount (word:y@(firstLetter:_):rest)
  | word == "the" && elem firstLetter vowels = 1 + myCount (y:rest)
  | otherwise = 0 + myCount (y:rest)
myCount _ = 0

{-ex3 vowels in a word-}

vowelsInWord :: String -> Int
vowelsInWord [] = 0
vowelsInWord (x: rest)
  | elem x vowels = 1 + vowelsInWord rest
  | otherwise = vowelsInWord rest

{-ex Validate the word-}

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x
  | vowelsInWord x > div (length x) 2 = Nothing
  | otherwise = Just (Word' x)

{-ex It's only Natural-}

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = (natToInteger x) + 1

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just (Succ (maybe Zero id (integerToNat (x - 1))))

intToNatAlternative :: Integer -> Maybe Nat
intToNatAlternative x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just $ Succ $ fromMaybe Zero $ intToNatAlternative $ x - 1

