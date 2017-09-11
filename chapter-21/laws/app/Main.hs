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
  foldr f d (Cons x rest) = f x (foldr f d rest)

  foldMap ctor (Nil) = mempty
  foldMap ctor (Cons x rest) = ctor x `mappend` (foldMap ctor rest)

{-ex5-}
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

instance Foldable (Three a b) where
  foldr f d (Three x y c) = f c d

instance Traversable (Three a b) where
  traverse f (Three x y c) = fmap (Three x y) (f c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

{-ex6-}

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

instance Foldable (Three' a) where
  foldr f d (Three' a b b') = f b (f b' d)

instance Traversable (Three' a) where
  traverse f (Three' a b b') = liftA3 Three' (pure a) (f b) (f b')

{-ex7-}

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S n a') = S (fmap f n) (f a')

instance Foldable n => Foldable (S n) where
  foldMap ctor (S n a') = (foldMap ctor n) `mappend` (ctor a')

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = liftA2 S (traverse f n) (f a)

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = do
    a' <- arbitrary
    a <- arbitrary
    return (S (pure a') a)

instance (Eq a) => EqProp (S n a) where
  (=-=) (S n a) (S n' a') = eq a a'

{-ex8-}
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = frequency [
      (1, return Empty),
      (2, liftA3 Node arbitrary arbitrary arbitrary),
      (2, fmap Leaf arbitrary)
    ]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap f (Empty) = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t x t') = Node (fmap f t) (f x) (fmap f t')

instance Foldable Tree where
  foldMap ctor (Empty) = mempty
  foldMap ctor (Leaf a) = ctor a
  foldMap ctor (Node t a t') = (foldMap ctor t) `mappend` (ctor a) `mappend` (foldMap ctor t')

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = fmap Leaf (f a)
  traverse f (Node t a t') = liftA3 Node (traverse f t) (f a) (traverse f t')

{-main-}
main :: IO ()
main = do
  putStrLn "<<<<<Alias of []>>>>>"
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)

  putStrLn "<<<<<Identity>>>>>"
  let identityTrigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable identityTrigger)

  putStrLn "<<<<<Constant>>>>>"
  let constantTrigger = undefined :: Constant Int (Int, Int, [Int])
  quickBatch (traversable constantTrigger)

  putStrLn "<<<<<Maybe>>>>>"
  let maybeTrigger = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable maybeTrigger)

  putStrLn "<<<<<List>>>>>"
  let listTrigger = undefined :: List (Int, Int, [Int])
  quickBatch $ applicative (undefined :: List (String, Int, Char))
  quickBatch (traversable listTrigger)

  putStrLn "<<<<<Three>>>>>"
  let threeTrigger = undefined :: Three Int Int (Int, Int, [Int])
  quickBatch (traversable threeTrigger)

  putStrLn "<<<<<Three'>>>>>"
  let threeTrigger' = undefined :: Three' Int (Int, Int, [Int])
  quickBatch (traversable threeTrigger')

  putStrLn "<<<<<SuckOne>>>>>"
  let suckTrigger = undefined :: S (Maybe) (Int, Int, [Int])
  quickBatch (traversable suckTrigger)

  putStrLn "<<<<<Tree>>>>>"
  let treeTrigger = undefined :: Tree (Int, Int, [Int])
  quickBatch (functor treeTrigger)
  quickBatch (traversable treeTrigger)

