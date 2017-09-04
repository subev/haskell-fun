module LastExercises where

import Control.Monad (join, liftM, liftM2, forM)

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
{-TODO implement with known functions-}
meh = forM

flipType :: (Monad m) => [m a] -> m [a]
{-TODO implement with known functions-}
flipType = sequence
