module CasePractise where

functionc :: Ord a => a -> a -> a
functionc x y = case x > y of
                  True -> x
                  False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 x = case even x of
                 True -> x + 2
                 False -> x

mycompare :: Int -> Int
mycompare x = case compare x 0 of
          LT -> -1
          GT -> 1
          EQ -> 0
