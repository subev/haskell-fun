module Currying where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 6

subtractStuff a b = a - b - 5

ignoreArguments x y z = "Amazing"

sumOfTuple = uncurry (+)
{-sumOfTuple (1,2)-}

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f a b = f (a,b)
curriedFirst = myCurry fst

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a,b) = f a b
unCurriedAdd = myUncurry (+)

unCurryThreeArgs :: (a->b->c->d) -> (a, b, c) -> d
unCurryThreeArgs f (a,b,c) = f a b c

curryThreeArgs :: ((a,b,c) -> d) -> a -> b -> c -> d
curryThreeArgs f a b c = f (a,b,c)

addThreeNumbers :: Num a => a -> a -> a -> a
addThreeNumbers x y z = x + y + z

addThreeNumbersUncurried :: Num a => (a,a,a) -> a
addThreeNumbersUncurried = unCurryThreeArgs addThreeNumbers

makeThreeNumbersCurried :: Num a => a -> a -> a -> a
makeThreeNumbersCurried = curryThreeArgs addThreeNumbersUncurried

{-Exercises-}
{-foo :: a -> a -> a-}
{-foo a b = a b-}
