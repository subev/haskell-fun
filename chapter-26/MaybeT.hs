{-# LANGUAGE InstanceSigs #-}
module MaybeT where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT mmab) <*> (MaybeT mma) = MaybeT $ (liftA2 . liftA2) ($) mmab mma
  {-the book again suggests some harder to me approach-}
  {-(MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma-}

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: (MaybeT m a) -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $
    mma >>= \ma ->
      case ma of
        Nothing -> return Nothing
        Just a -> runMaybeT (f a)

{-or shorter as in the book-}
  (MaybeT mma) >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a -> runMaybeT (f a)

instance MonadTrans MaybeT where
  lift = MaybeT . fmap return
