module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Traversable

import Control.Applicative

type TI = []

{-ex1-}
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad (Identity) where
  return = pure
  (>>=) (Identity x) f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)

{-ex2-}
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant (a)

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty
  foldr f d (Constant x) = d

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (mappend a b)

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

{-ex3-}
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse f Nada = pure $ Nada
  traverse f (Yep a) = fmap Yep (f a)

instance Functor (Optional) where
  fmap _ (Nada) = Nada
  fmap f (Yep x) = Yep (f x)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (3, return (Yep a))]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

{-ex4-}
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil), (1, return (Cons a b) )]

instance Eq a => EqProp (List a) where
  (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil

  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f rest) ys = (fmap f ys) `append` (rest <*> ys)

instance Traversable (List) where
  traverse f Nil = pure $ Nil
  traverse f (Cons a rest) = liftA2 (Cons) (f a) (traverse f rest)

instance Foldable List where
  foldr f d Nil = d
  foldr f d (Cons x rest) = foldr f (f x d) rest

main :: IO ()
main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
  let identityTrigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable identityTrigger)
  let constantTrigger = undefined :: Constant Int (Int, Int, [Int])
  quickBatch (traversable constantTrigger)
  let maybeTrigger = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable maybeTrigger)
  {-TODO THIS TEST DOES NOT PASS :((-}
  let listTrigger = undefined :: List (Int, Int, [Int])
  quickBatch (traversable listTrigger)

