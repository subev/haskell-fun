module Recursion where

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n-1) * n

plusOne :: Int -> Int
plusOne = (+1)

applyTimes :: (a -> a) -> Int -> a -> a
applyTimes f 0 p = p
applyTimes f n p = f (applyTimes f (n-1) p)

{-recreated this on my own-}
applyTimes2 :: (a -> a) -> Int -> (a -> a)
applyTimes2 f 0 = id
applyTimes2 f n = f . applyTimes f (n-1)

f :: Bool -> Maybe Int
f False = Just 0
f True = Nothing

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

{--------------------}

type Numerator = Integer
type Denominator = Integer
data Result = Result (Integer, Integer) | DevideByZero
  deriving Show


divideBy :: Numerator -> Denominator -> Result
divideBy x 0 = DevideByZero
divideBy x y = devide x y 0 where
  devide x y count
    | x < y = Result (count, x)
    | otherwise = devide (x - y) y (count + 1)

sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = sumTo (n - 1) + n

multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum 0 y = 0
multiplyBySum 1 y = y
multiplyBySum x y = y + multiplyBySum (x - 1) y

{-how cool is that huh?!-}
multiplyByApplyingSum :: Int -> Int -> Int
multiplyByApplyingSum x y = applyTimes (+x) y 0

mcCarthy :: Int -> Int
mcCarthy x
  |x > 100 = x - 10
  |otherwise = mcCarthy . mcCarthy $ (x + 11)
