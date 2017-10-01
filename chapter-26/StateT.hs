{-# LANGUAGE InstanceSigs #-}
module StateT where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> (StateT s m a) -> (StateT s m b)
  fmap f (StateT smp) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) smp

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (f, s') <- (smf s)
    {-the tricky part here is that we need to get the
    first state out and then feed it to the second monad-}
    (a, s'') <- (sma s')
    pure $ (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: (StateT s m a) -> (a -> (StateT s m b)) -> (StateT s m b)
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- (sma s)
    let smb = (runStateT (f a))
     in (smb s')


instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
