module FoldsExercises where

{-ex1-}
stops  = "pbtdkg"
vowels = "aeiou"

stopVowelStop = [ (x,y,z) | x <- stops, y <- vowels, z <- stops]

stopVowelStopWithP = [ (x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["chair", "table", "cook"]
verbs = ["codes", "runs", "smiles"]

tuples = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

{-ex2-}
{-averageLengthOfWords-}
{-averageLengthOfWords :: String -> Double-}
averageLengthOfWords x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))


myOr :: [Bool] -> Bool
myOr = foldr (||) False

{- here `const` helps us to return a function (from the paranthesis)
that waits for one more (extra) argument which will be ignored
if there is no const the expression in the brackets will return
a function that waits for 1 argument instead of 2 as explained above -}

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (const . f)  False

mySome :: (a -> Bool) -> [a] -> Bool
mySome f = foldr (\x _ -> f x)  undefined


myElem :: Eq a => a -> [a] -> Bool
myElem i = foldr (\x y -> if x == i then True else y) False

myElemUsingAny :: Eq a => a -> [a] -> Bool
myElemUsingAny x = any (x==)

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x) : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if (f x y) == GT then x else y) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if (f x y) == LT then x else y) (head xs) xs
