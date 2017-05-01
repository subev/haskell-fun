module MyFunction where

import Data.List

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x then myAnd xs else False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) = x == y || myElem x ys


myElem_UseAny :: (Eq a) => a -> [a] -> Bool
myElem_UseAny x ys = any (\y -> x == y) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaxBy :: (a -> a -> Ordering) -> [a] -> a
myMaxBy f (x:[]) = x
myMaxBy f (x:xs) = if f x y == GT
                      then x
                      else y
                        where y = myMaxBy f xs

myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy f (x:[]) = x
myMinBy f (x:xs) = if f x y == LT
                      then x
                      else y
                        where y = myMinBy f xs

myMin :: (Ord a) => [a] -> a
myMin xs = myMinBy compare xs

myMax :: (Ord a) => [a] -> a
myMax xs = myMaxBy compare xs
