module PropertyTestingExercises where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Char

half :: Double -> Double
half x = x / 2

halfIdentity = (*2) . half

{-2nd exercise-}
{-'y' below holds the next value in the list-}
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z =
  x + ( y + z ) == (x + y) + z

plusCommutative x y = x + y == y + x

mulAssociative x y z =
  x * ( y * z ) == (x * y) * z

powerAssociative :: Int -> Int -> Bool
powerAssociative x y =
  x ^ y == y ^ x

mulCommutative x y = x * y == y * x

quotProve x y = (quot x y)*y + (rem x y) == x
divProve x y = (div x y)*y + (mod x y) == x

reversingTwiceIsId :: [Int] -> Bool
reversingTwiceIsId xs =
  (reverse $ reverse xs) == id xs

{-dollarSign :: (Int -> Char) -> Int -> Bool-}
{-dollarSign f x =-}
  {-(f x) == (f $ x)-}

main :: IO ()
main = hspec $ do

  describe "1.dividing by 2" $ do
    it "returns the same number when adding two results" $ do
      property $ \x -> (half x) + (half x) == x
    it "halfIdentity" $ do
      property $ \x -> x == halfIdentity x

  describe "2.native sort works" $ do
    it "works with the provided property function and array of int" $ do
      property $ listOrdered . (sort :: [Int] -> [Int])
    it "works with the provided property function and array of char" $ do
      property $ listOrdered . (sort :: [Char] -> [Char])

  describe "3.test sum for associativity and commutativity" $ do
    it "works for associativity" $ do
      property $ (plusAssociative :: Int -> Int -> Int -> Bool)
    it "works for commutativity" $ do
      property $ (plusCommutative :: Int -> Int -> Bool)

  describe "4.test multiplication for associativity and commutativity" $ do
    it "works for associativity" $ do
      property $ (mulAssociative :: Int -> Int -> Int -> Bool)
    it "works for commutativity" $ do
      property $ (mulCommutative :: Int -> Int -> Bool)

  {-describe "5.prove quot and rem" $ do-}
    {-it "quot" $ do-}
      {-property $ (quotProve :: Int -> Int -> Bool)-}
    {-it "rem" $ do-}
      {-property $ (divProve :: Int -> Int -> Bool)-}

  {-describe "6.test ^ for associativity and commutativity" $ do-}
    {-it "works for associativity" $ do-}
      {-property $ powerAssociative-}

  describe "7.test reversing a list twice is the same as id" $ do
    it "reversing twice" $ do
      property $ reversingTwiceIsId

  {-describe "8.dollar operator ($)" $ do-}
    {-it "works for $" $ do-}
      {-property $ dollarSign-}
      {-{-property $ ((\f -> \x -> (f x) == (f $ x)) :: (Int -> Bool) -> Int -> Bool)-}-}
    {-Not sure how to test a function that is supposed to be generated?-}

  describe "9.fold method" $ do
    it "foldr (:) == (++)" $ do
      {-notice the question was if they are the same,
         but I have flipped the arguments to make the test pass-}
      property ((\xs -> \ys -> flip (foldr (:)) xs ys == (++) xs ys) :: [Int] -> [Int] -> Bool)
    it "foldr (++) [] == concat" $ do
      property ((\xs -> (foldr (++) [] xs) == concat xs) :: [[Int]] -> Bool)

  describe "10. f n xs = length (take n xs) == n" $ do
    it "damn it almost works" $ do
      property ((\n -> \xs -> length (take n xs) == n) :: Int -> [Int] -> Bool)

  describe "Read and Show" $ do
    it "piping read and show is id" $ do
      property ((\x -> read (show x) == x) :: String -> Bool)

  describe "some failing tests" $ do
    it "floating issues" $ do
      property ((\x -> (sqrt (x * x)) == x) :: Double -> Bool)

  describe "12.idempotency" $ do
    it "twice and four times calling capitalizeWord" $ do
      property capitalizingFirstConfirm
    it "twice and four times calling sort" $ do
      property sortConfirm

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizingFirstConfirm :: String -> Bool
capitalizingFirstConfirm x =
  (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

sortConfirm :: [Int] -> Bool
sortConfirm x =
  (sort x == twice sort x) && (sort x == fourTimes sort x)
