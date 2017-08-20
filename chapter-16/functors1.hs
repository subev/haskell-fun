module Functors1 where

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)


{-<$> is same as $ but for Functor like structures-}
{-i.e-}
{-(+1) <$> Just 1-}
{-is same as-}
{-(+1) $ 1-}

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

{-breaking the composition law of Functor-}
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

shouldBeFalse = (fmap ((++ "lol") . (++ "foo")) (Heisenberg 1 "bar"))
  == ((fmap (++ "lol") . fmap (++ "foo")) (Heisenberg 1 "bar"))

{-this is the right way-}
{-instance Functor CountingBad where-}
  {-fmap f (Heisenberg n a) = Heisenberg (n) (f a)-}

