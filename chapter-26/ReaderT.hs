{-# LANGUAGE InstanceSigs #-}
module ReaderT where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT (\_ -> pure x)
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $
    \r -> liftA2 ($) (rmab r) (rma r)

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- (rma r)
    (runReaderT (f a)) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const
