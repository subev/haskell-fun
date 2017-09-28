{-# LANGUAGE InstanceSigs #-}
module Practise where

import Control.Applicative (liftA2, (<*>))

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose (pure (pure x))

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose ((liftA2 . liftA2) ($) f a)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = ((foldMap . foldMap) f fga)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  {-TODO THIS IS CRAZY, I admit checked the sources-}
  traverse f (Compose fga) = fmap Compose (traverse (traverse f) fga)

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

{-ex1-}
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

{-ex2-}
data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

{-ex3-}
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

{-ex4-}
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)

{-ex5-}
data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

{-ex6-}
data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

{-ex7-}
data EEither a b = LLeft a | RRight b deriving (Eq, Show)

instance Bifunctor EEither where
  bimap f _ (LLeft a) = LLeft (f a)
  bimap _ g (RRight b) = RRight (g b)
