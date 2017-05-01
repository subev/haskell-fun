module AssertingTypes where

{-multiBy10 :: Int -> Int-}
multiBy10 = compose multiby5 multiby2
  where multiby5 = (*) 5 {-:: Int -> Int-}
        multiby2 = (*) 2 {-:: Int -> Int-}

triple x = tripleItYo x
  where tripleItYo:: Int -> Int
        tripleItYo y = (*) y 3

compose :: (a2 -> a3) -> (a1 -> a2) -> (a1 -> a3)
compose f1 f2 = \x -> f1 (f2 x)
