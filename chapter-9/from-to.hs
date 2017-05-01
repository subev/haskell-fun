module FromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x == y = [x]
  | otherwise = [False,True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x == y = [x]
  | x == LT = [x] ++ eftOrd EQ y
  | x == EQ = [x] ++ eftOrd GT y
  |otherwise = []

eftIntOrChar :: (Enum a, Ord a) => a -> a -> [a]
eftIntOrChar x y
  | x > y = []
  | otherwise = [x] ++ eftIntOrChar (succ x) y

