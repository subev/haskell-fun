module ApplicativePlaygroung where

import Control.Applicative
import Data.List (elemIndex)

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g = flip lookup $ [(7, "sup?"), (8, "chris"), (9, "aloha")]

h = flip lookup $ [(2, 3), (5, 6), (7, 8)]
m = flip lookup $ [(4, 10), (8, 13), (1, 9001)]

{-exercises-}
{-ex1-}
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

{-ex2-}
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

{-ex3-}
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

{-ex4-}
xs = [1..3]
ys = [4..6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x4 <*> y4)

{-ex-}
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)

{-ex-}
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant (a)

instance Monoid a => Applicative (Constant a) where
  {-TODO do not how to satisfy the compiler here-}
  pure x = undefined
  (<*>) (Constant a) (Constant b) = Constant (mappend a b)

{-ex-}
foo = const <$> Just "Hello" <*> pure "World"

{-ex-}
bar = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

{-interesting property of the ($) operator !! with this you can invert the order in which the
  Applicative structures should appear, i.e. the argument comes first and then the function  -}
shouldBeTrue = ($ 2) (+1) == ((+1) $ 2)
shouldAlsoBeTrue = (Just (+2) <*> pure 2) == (pure ($ 2) <*> Just (+2))
andThisShouldAlsoBeTrue = (pure ($ 1) <*> [(+1), (*2)]) == ([(+1), (*2)] <*> pure 1)
