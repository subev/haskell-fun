module ScanFib where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

first20fibs = take 20 $ fibs
fibsLessThan100 = takeWhile ((>) 100) fibs

factorials x = reverse $ scanr (*) 1 [1..x]
factorialN x = last $ factorials x
