module Exercises where

import Control.Applicative
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-ex1-}
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
  pure x = Identity x
  (<*>) (Identity f) x = fmap f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

{-ex2-}
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

{-ex3-}
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two x f) (Two y a) = Two (x `mappend` y) (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

{-ex4-}
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y c) = Three x y (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b c) (Three a' b' c') = Three (a `mappend` a') (b `mappend` b') (c c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

{-ex5-}
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a b c) (Three' a' b' c') = Three' (a `mappend` a') (b b') (c c')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

{-ex6-}
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c d) (Four a' b' c' d') =
    Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (d d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

{-ex7-}
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a b c d) (Four' a' b' c' d') =
    Four' (a `mappend` a') (b `mappend` b') (c `mappend` c') (d d')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  putStr "Identity"
  quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
  putStr "Pair"
  quickBatch $ applicative (undefined :: Pair (Int, Int, Int))
  putStr "Twoo"
  quickBatch $ applicative (undefined :: Two (Sum Int) (Int, Int, Int))
  putStr "Three"
  quickBatch $ applicative (undefined :: Three (Sum Int) (Sum Int) (Int, Int, Int))
  putStr "Three'"
  quickBatch $ applicative (undefined :: Three' (Sum Int) (Int, Int, Int))
  putStr "Four"
  quickBatch $ applicative (undefined :: Four (Sum Int) (Sum Int) (Sum Int) (Int, Int, Int))
  putStr "Four'"
  quickBatch $ applicative (undefined :: Four' (Sum Int) (Int, Int, Int))
