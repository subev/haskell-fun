module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

myTensDigit :: Integral a => a -> a
myTensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

hunsD = myTensDigit . flip div 10

foldBool :: a -> a -> Bool -> a
foldBool x y flag
  |flag = x
  |otherwise = y

g :: (a->b) -> (a,c) -> (b,c)
g f (a,c) = ((f a), c)

{-you can call it like-}
{-g (\x->3) (99,5)-}

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

ex6 :: (Show a, Read b) => a -> b
ex6 = read . show

ex6Int :: (Show a) => a -> Int
ex6Int = ex6

main = do
  print (roundTrip 4)
  print (id 4)
