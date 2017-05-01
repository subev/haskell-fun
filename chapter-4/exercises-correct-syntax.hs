module Exercises where

x = (+)

f xs = w `x` 1
  where w = length xs

ex3 = \xs -> head xs

ex4 :: (a,b) -> a
ex4 = \t -> fst t

