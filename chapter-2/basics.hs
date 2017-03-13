module LetExpressions where

printInc n = print plusOne
  where plusOne = n + 1

printInc2 n = let plusTwo = n + 2
               in print plusTwo

mult1 = x * y
  where x = 5
        y = 3

{-exercise 1-}
foo = let x = 3; y = 1000 in x * 3 + y

ex1 = x * 3 + y
  where x = 3
        y = 1000

{-some extra playground-}
ex11 = (\x -> \y -> x * 3 + y) 3 1000
ex111 = \x -> \y -> x * 3 + y

{-exercise 2-}
bar = let y = 10; x = 10 * 5 + y in x * 5

ex2 = x * 5
  where y = 10
        x = 10 * 5 + y

{-exercise 3-}
baz = let x = 7; y = negate x; z = y * 10;  in z / x + y

ex3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10
