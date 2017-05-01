module ExercisesLists where
import Data.Char

getUppers = filter isUpper

capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs

onlyFirstCapital = toUpper . head
