module FoldableExercises where

import Data.Monoid

{-ex1-}
data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty

  foldr f d (Constant x) = d

{-ex2-}
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b
  foldr f d (Two a b) = f b d

{-ex3-}
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

{-ex4-}
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = mappend (f b) (f b')

{-ex5-}
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = mappend (mappend (f b) (f b')) (f b'')
  foldr f d (Four' a b b' b'') = f b'' $ f b' $ f b d


{-thinking cap mode-}
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF predicate = foldMap (\x -> case (predicate x) of
                                     True -> pure x
                                     False -> mempty)

filterFExample = filterF (>2) [-1, 1, 2, 3, 5, -6] :: [Int]
