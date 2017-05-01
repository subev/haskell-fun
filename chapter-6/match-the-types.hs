module MatchTheTypes where

import Data.List

i :: Num a => a
i = 1

f :: Float
f = 1.0

freud :: a -> a
freud x = x

{-I do not understand this execsises-}
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

jungInt :: [Int] -> Int
jungInt xs = head (sort xs)
