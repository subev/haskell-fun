module Foldable where

import Data.Monoid

{-Identity-}
data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

{-Maybe-}

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

{-usage -}
fmMaybeResult = foldMap (+1) (Just 1) :: Sum Int

{-exercises-}
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem target = foldr (\x _ -> x == target) False

isMin :: Ord a => a -> Maybe a -> Maybe a
isMin y Nothing = Just y
isMin y (Just x)
  | x < y = Just x
  | otherwise = Just y

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr isMin Nothing

isMax :: Ord a => a -> Maybe a -> Maybe a
isMax y Nothing = Just y
isMax y (Just x)
  | x > y = Just x
  | otherwise = Just y

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr isMax Nothing

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> True) False

length :: (Foldable t) => t a -> Int
length = foldr (\_ y -> y + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap ctor = foldr (mappend . ctor) mempty
