module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = "unknown"


digits :: Int -> [Int]
digits x
  |x < 10 = x :[]
  |otherwise = (digits $ div x 10) ++ (mod x 10 :[])

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
