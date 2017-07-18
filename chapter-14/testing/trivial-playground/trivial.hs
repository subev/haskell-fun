module Trivial where

import Test.QuickCheck

trivialInt :: Gen Int
trivialInt = return 1
{-sample' trivialInt-}

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]
{-sample' oneThroughThree-}

genBool :: Gen Bool
genBool = choose (False, True)
{-sample' genBool-}

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
{-sample genTuple (:: Gen (Int, Double))-}

genThriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThriple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)
{-sample' (genThriple :: Gen (Char, Int, Ordering))-}

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]


{-use 2nd generator 5 times more often compared to first generator-}
{-sample' $frequency [(1, elements [1..3]), (5, elements [8..10])]-}

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]
{-sample' (genMaybe :: Gen (Maybe Int))-}


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
