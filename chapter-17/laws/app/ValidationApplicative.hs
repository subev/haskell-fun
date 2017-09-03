module ValidationApplicative where

import Control.Applicative

data Sum a b = First a | Second b deriving (Eq, Show)

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) (First x) _ = First x
  (<*>) (Second _) (First x) = First x
  (<*>) (Second f) (Second a) = Second (f a)

-- same as Sum/Either
instance Functor (Validation e) where
  fmap f (Error e) = Error e
  fmap f (Success a) = Success $ f a

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure x = Success x
  (<*>) (Error e) (Success s) = Error e
  (<*>) (Success s) (Error e) = Error e
  (<*>) (Error e) (Error e') = Error $ e `mappend` e'
  (<*>) (Success f) (Success a) = Success $ f a
