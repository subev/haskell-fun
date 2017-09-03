module MyList where

import Control.Applicative

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



instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f rest) ys = (fmap f ys) `append` (rest <*> ys)


{-example when the above is implemented-}
functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)
result = (functions <*> values) == (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))))


