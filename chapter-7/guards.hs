module Guards where

myAbs :: Integer -> Integer
myAbs x               {-notice there is no '=' sign here-}
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "Too Low"
  | x > 145 = "Too Low"
  | otherwise = "Just right man"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  |a^2 + b^2 == c^2 = "Right ON!"
  |otherwise = "not right"

averageGrade :: (Fractional a, Ord a) => a -> Char
averageGrade x
  |y > 0.9 = 'A'
  |y > 0.8 = 'B'
  |y > 0.7 = 'C'
  |y >= 0.59 = 'D'
  |otherwise = 'F'
  where y = x / 100

numbers :: (Ord a, Num a) => a -> Integer
numbers x
  |x < 0 = -1
  |x == 0 = 0
  |x > 0 = 1
