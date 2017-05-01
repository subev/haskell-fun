module Lists where

ex4 = 6 `div` length [1,2,3] {-using / does type mismatch-}


{-ex7-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

{-ex9-}
{-do not forget to wrap in brackets the negative number when applying the function-}
myAbs :: Int -> Int
myAbs x =
  if x < 0
     then negate x
  else
    x

{-ex10-}
f :: (a,b) -> (c,d) -> ((b,d), (a, c))
f = \(a,b) -> \(c,d) -> ((b,d), (a, c))

f1 :: (a,b) -> (c,d) -> ((b,d), (a, c))
f1 = \a1 -> \a2 -> ((snd a1, snd a2), (fst a1, fst a2))

