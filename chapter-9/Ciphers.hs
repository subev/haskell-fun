module Ciphers where

import Data.Char

cipher :: Int -> String -> String
cipher offset = transform offset newPosition

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
