module ListComprehensions where
import Data.Char

powerOn :: Int -> [Int] -> [Int]
powerOn p xs = [x^p | x <- xs]

numberInTouplesTo300 = [(x,y,z) | x <- [0..2], y <- [0..9], z <- [0..9]]

noCapitals str = [x | x <- str, not $ elem x ['A'..'Z']]


mySqrt = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

powerTuples = [(x,y) | x <- mySqrt, y <- myCube, x < 50, y < 50]

mylen :: [a] -> Integer
mylen [] = 0
mylen (x: xs) = 1 + mylen (xs)

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x :  mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs)
  | f x = x : (myfilter f xs)
  | otherwise = myfilter f xs

articleFilter :: String -> [String]
articleFilter = noArticles . words
  where
    noArticles :: [String] -> [String]
    noArticles = filter (not .flip elem ["the", "a", "an"] . map Data.Char.toLower)

{-zip exercises-}
myzip :: (a->b->c) -> [a] -> [b] -> [c]
myzip _ _ [] = []
myzip _ [] _ = []
myzip f (x:xs) (y:ys) = (f x y) : myzip f xs ys

zzip :: [a] -> [b] -> [(a,b)]
zzip = myzip (\a -> \b -> (a,b))
