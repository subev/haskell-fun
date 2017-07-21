module Main where

import Test.QuickCheck
{-this is required for sum types-}
import Test.QuickCheck.Gen (oneof)
{-import CoArbitrary-}

{-----------------------------------------}
data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = do
  sample trivialGen


{-----------------------------------------}

data Identity a =
  Identity a deriving (Eq, Show)

identityGen :: Arbitrary a =>
               Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a =>
  Arbitrary (Identity a) where
    arbitrary = identityGen

identityIntGen :: Gen (Identity Int)
identityIntGen = identityGen

{-----------------------------------------}
{-Generators for Product types-}

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) =>
           Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Pair a b) where
    arbitrary = pairGen

pairCharDoubleGen :: Gen (Pair Char Double)
pairCharDoubleGen = pairGen

{-----------------------------------------}
{-Generators for Sum types-}

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) =>
                Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b) =>
                  Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
            (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

{-----------------------------------------}
{-exercises-}
data Fool = Fulse | Frue deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual =
  oneof [return $ Frue, return $ Fulse]

foolGenThreeToOne :: Gen Fool
foolGenThreeToOne =
  frequency [(3, return $ Frue), (1, return $ Fulse)]

instance Arbitrary Fool where
  arbitrary = foolGenEqual
