module PropertyTestingExercises where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

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

main :: IO ()
main = hspec $ do
  describe "dividing by 2" $ do
    it "returns the same number when adding two results" $ do
      property $ \x -> (half x) + (half x) == x
    it "halfIdentity" $ do
      property $ \x -> x == halfIdentity x
  describe "native sort works" $ do
    it "works with the provided property function and array of int" $ do
      property $ listOrdered . (sort :: [Int] -> [Int])
    it "works with the provided property function and array of char" $ do
      property $ listOrdered . (sort :: [Char] -> [Char])


