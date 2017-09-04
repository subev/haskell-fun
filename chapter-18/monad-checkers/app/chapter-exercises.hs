module ChapterExercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-ex1-}
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) NopeDotJpg f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

{-ex2-}
data PhhhbbtttEither a b = Leftt a | Rightt b deriving (Eq, Show)

instance Functor (PhhhbbtttEither a) where
  fmap f (Leftt a) = Leftt a
  fmap f (Rightt b) = Rightt $ f b

instance Applicative (PhhhbbtttEither a) where
  pure = Rightt
  (<*>) (Leftt a) _ = Leftt a
  (<*>) _ (Leftt a) = Leftt a
  (<*>) (Rightt f) (Rightt a) = Rightt $ f a

instance Monad (PhhhbbtttEither a) where
  return = pure
  (>>=) (Leftt x) _ = Leftt x
  (>>=) (Rightt x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Leftt a), (1, return $ Rightt b)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq

{-ex3-}

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

{-ex4-}
data List a = Nil | Cons a (List a) deriving (Eq, Show)

{-helper function-}
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys
{-endof helper function-}

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative (List) where
  pure x = Cons x Nil

  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f rest) ys = (fmap f ys) `append` (rest <*> ys)

instance Monad (List) where
  return = pure
  (>>=) (Nil) _ = Nil
  (>>=) (Cons x rest) f = (f x) `append` (rest >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil), (1, return (Cons a b) )]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  putStr "ex1: NopeDotJpg"
  let ex1type = (undefined :: Nope (Int, Int, Int))
  quickBatch $ functor ex1type
  quickBatch $ applicative ex1type
  quickBatch $ monad ex1type

  putStr "ex2: RighttLeftt"
  let ex2type = (undefined :: PhhhbbtttEither Int (Int, Int, Int))
  quickBatch $ functor ex2type
  quickBatch $ applicative ex2type
  quickBatch $ monad ex2type

  putStr "ex3: Identity"
  let ex3type = (undefined :: Identity (Int, Int, Int))
  quickBatch $ functor ex3type
  quickBatch $ applicative ex3type
  quickBatch $ monad ex3type

  putStr "ex4: List"
  let ex4type = (undefined :: List (Int, Int, Int))
  quickBatch $ functor ex4type
  quickBatch $ applicative ex4type
  quickBatch $ monad ex4type
