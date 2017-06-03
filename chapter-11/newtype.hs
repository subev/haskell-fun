module NewType where

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show)
newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoatsRefactored :: Goats -> Bool
tooManyGoatsRefactored (Goats x) = x > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

{-GeneralizedNewtypeDeriving-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
class TooMany2 a where
  tooMany2 :: a -> Bool

instance TooMany2 Int where
  tooMany2 n = n > 42

newtype Goats2 = Goats2 Int deriving (Show, Eq)

{-instance TooMany2 Goats2 where-}
  {-tooMany2 (Goats2 n) = tooMany n-}

{-ex1-}
newtype Goats3 = Goats3 (Int, String) deriving (Show, Eq)
