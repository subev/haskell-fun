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
{-this is basically implementation of forM-}
{-meh = forM-}
meh (a: []) f = (flip (:) []) <$> (f a)
meh (a: rest) f = (((:) <$> (f a)) <*> (meh rest f))

flipType :: (Monad m) => [m a] -> m [a]
{-this is basically implementation of sequence-}
{-flipType = sequence-}
flipType [] = pure [];
flipType (ma: rest) = (fmap (:) ma) <*> (flipType rest)
