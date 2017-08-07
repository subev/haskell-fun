module Main where

import Data.Monoid
import Test.QuickCheck
import Control.Monad

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool

detailedAssociativeCheck :: IO ()
detailedAssociativeCheck = verboseCheck (monoidAssoc :: S -> S -> S -> B)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (x <> mempty) == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (mempty <> x) == x

data Bull  = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMAppend = Bull -> Bull -> Bull -> Bool

data Optional a = Only a | Nada deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mappend Nada Nada = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (x <> y)

  mempty = Nada

main :: IO ()
main = do
  detailedAssociativeCheck
  quickCheck (monoidAssoc :: S -> S -> S -> B)
  quickCheck (monoidAssoc :: BullMAppend)
  quickCheck (monoidRightIdentity :: Bull -> Bool) {-this and next are failing-}
  quickCheck (monoidLeftIdentity :: Bull -> Bool)





{-ex maybe-}

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  {-if the first argument is not Nada i.e. it contains Only, then return it-}
  mappend x@(First' (Only _)) y = x
  {-otherwise return the last argument-}
  mappend _ y = y

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool


instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
  a <- arbitrary
  frequency [
      (5, return $ First' (Only a)),
      (1, (return $ First' Nada))
    ]

main2 :: IO ()
main2 = do
  quickCheck (monoidAssoc         :: FirstMappend)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
  quickCheck (monoidLeftIdentity  :: First' String -> Bool)
