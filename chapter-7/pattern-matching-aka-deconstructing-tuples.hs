module ExerciseTuples where

k (x,y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)

f :: (a,b,c) -> (d, e, f) -> ((a,d), (c, f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))

funZ x =
  case x + 1 == 1 of
    True -> "Awesome"
    False -> "wut"

palindrom xs =
  case xs == reverse xs of
    True -> "Palindrom"
    False -> "No palindrom"

palindrom' xs =
  case xs == reversed of
    True -> "Palindrom"
    False -> "No palindrom"
  where reversed = reverse xs
