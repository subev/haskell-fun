module TupleFunctors where

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Or a) where
  fmap f (First x) = First x
  fmap f (Second y) = Second (f y)
