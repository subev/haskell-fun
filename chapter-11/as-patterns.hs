module AsPatterns where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf xs@(x:restXs) ys =
  elem x ys && isSubsequenceOf restXs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map tupleWithCapitalized . words

tupleWithCapitalized :: String -> (String,String)
tupleWithCapitalized word@(head: rest) = (word, ((toUpper head): rest))

{-language execs-}
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeSentence :: String -> String
capitalizeSentence [] = []
capitalizeSentence text = capitalized ++ '.' : capitalizeSentence rest
  where capitalized = capitalizeWord $ takeWhile (/= '.') text
        (x: rest) = dropWhile (/= '.') text



