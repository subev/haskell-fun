module MyList where

import Control.Applicative
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-list applicative-}
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = (f h (fold f b t))

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f Nil = Nil
flatMap f x = concat' $ fmap f x



instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil), (1, return (Cons a b) )]

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f rest) ys = (fmap f ys) `append` (rest <*> ys)

instance Eq a => EqProp (List a) where
  (=-=) = eq


{-example to confirm the above implemented properly-}
functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
result = (functions <*> values) == (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))))


{-ziplist applicative-}

take' :: Int -> List a -> List a
take' n _
  | n < 1 = Nil
take' _ Nil = Nil
take' n (Cons x rest) = Cons x (take' (n - 1) rest)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys' where
    xs' = let (ZipList' l) = xs
           in take' 3000 l
    ys' = let (ZipList' l) = ys
           in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . pure
  (<*>) (ZipList' Nil) (ZipList' _) = ZipList' Nil
  (<*>) (ZipList' _) (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons f restf)) (ZipList' (Cons x resta)) = ZipList' $ Cons (f x) (restf <*> resta)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ ZipList' a )]


main :: IO ()
main = do
  putStr "Validating applicative List: "
  quickBatch $ applicative (undefined :: List (String, Int, Char))
  {-TODO this is not working-}
  putStr "Validating applicative ZipList': "
  quickBatch $ applicative (undefined :: ZipList' (String, Int, Char))

