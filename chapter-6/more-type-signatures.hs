add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
     then x + y
     else x

addInt :: Int -> Int -> Int
addInt = (+)

addWeird2 :: Int -> Int -> Int
addWeird2 x y =
  if x > 1
     then x + y
     else x

{-check' :: Int -> Int -> Bool-}
{-or we can use anonymous signature like this-}
check' = (==) :: Int -> Int -> Bool

