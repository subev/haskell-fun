module Exer1 where

mth x y z = x * y * z
mth2 = \x -> \y -> \z -> x * y * z

addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
  where f n = n + 1

addOneWhenOdd n =
  case odd n of
    True -> f n
    False -> n
  where f = \n -> n + 1

addFive x y = (if x > y then y else x) + 5

addFiveLambda = \x -> \y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x

myflip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
