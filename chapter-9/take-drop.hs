module TakeDrop where

splitBy :: Char -> String -> [String]
splitBy delimeter x
  |x == [] = []
  |otherwise = [word] ++ splitBy delimeter next
  where word = takeWhile (/= delimeter) x
        rest = drop (length word) x
        next = dropWhile (== delimeter) rest

{-ex 2-}
firstSent = "Tyger Tuger, burning bright\n"
secondSen = "In the forests of the night \n"
thirdSen = "What immportal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSent ++ secondSen ++ thirdSen ++ fourthSen


