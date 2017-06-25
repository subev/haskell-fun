module Ciphers where

import Data.Char
import Control.Monad (forever)

cipher :: Int -> String -> String
cipher offset = transform offset newPosition

decode :: Int -> String -> String
decode offset = cipher (-offset)

decoode :: Int -> String -> String
decoode offset = transform offset reversePosition

transform :: Int -> (Int -> Int -> Int) -> String -> String
transform offset f = map (chr . f offset . ord)

newPosition :: Int -> Int -> Int
newPosition offset x
  | (x + offset) > ord 'z' = ord 'A' + overflow - 1
  | otherwise = x + offset
    where overflow = x + offset - ord 'z'

reversePosition :: Int -> Int -> Int
reversePosition offset x
  | (x + offset) < ord 'A' = ord 'z' - overflow + 1
  | otherwise = x + offset
    where overflow = ord 'A' - (x + offset)

main :: IO ()
main = forever $ do
  putStrLn "Using offset of 3, now enter the string:"
  userInput <- getLine
  putStrLn $ "the encoded version is '" ++ cipher 3 userInput ++ "'"
  putStrLn $ "the decoded version is '" ++ (decode 3 $ cipher 3 userInput) ++ "'"
  putStrLn $ "the decoded version is '" ++ (decoode 3 $ cipher 3 userInput) ++ "'"
