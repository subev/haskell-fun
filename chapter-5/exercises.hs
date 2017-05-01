module Exercises where

exC = head [(0::Integer, "Doge"),(1,"Kitty")]
exD = if False then True else False
exE = length [1,2,3,4]
exF = length [1,2,3,4] > (length "TOCAT")

{-This is how you do array destructuring-}
functionH :: [a] -> a
functionH (x:y:_) = y

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a,b) -> b
functionS (a,b) = b
