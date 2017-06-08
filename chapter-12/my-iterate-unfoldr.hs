module IterateAndUnfoldr where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = result : (myIterate f result)
  where result = f x

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case (f x) of
    {-next line 'a' is what will be inserted in the outpt array-}
    {-and 'b' is what x will be on next iteration-}
    (Just (a, b)) -> a : (myUnfoldr f b)
    (Nothing) -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr newF x
  where newF y = Just (y, f y)
