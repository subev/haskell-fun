{-# LANGUAGE RankNTypes #-}

module Natural where

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]
