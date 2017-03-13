module FunWithFun where

z = 7
x = y ^ 2
waxOn1 = x * 5
y = z + 8

waxOn2 = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

waxOn3 = let z = 7; y = z + 8; x = y ^ 2 in x * 5

tripleLambda = \x -> x * 3
triple x = x * 3


waxOff x = triple x
