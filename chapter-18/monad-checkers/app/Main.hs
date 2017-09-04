module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-bad monad-}

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  {-if incrementing i, it does not obey the identity law-}
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  {-if instead of (n + n') you have (n + 1) this part with incrementing also does not obey the law-}
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure

  (CountMe n a) >>= f =
    let (CountMe n' b) = f a
     in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main :: IO ()
main = do
  let toBeTest = (undefined :: CountMe (Int, Int, Int))
  quickBatch $ functor toBeTest
  quickBatch $ applicative toBeTest
  quickBatch $ monad toBeTest
