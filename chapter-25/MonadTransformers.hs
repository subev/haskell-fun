{-# LANGUAGE InstanceSigs #-}
module MonadTransformers where

import Control.Applicative (liftA2, (<*>))
import Control.Monad (join)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative m) => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT mab) <*> (IdentityT ma) = IdentityT $ mab <*> ma

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) :: (IdentityT m a) -> (a -> IdentityT m b) -> (IdentityT m b)
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
  {-same as-}
  {-(IdentityT ma) >>= f = IdentityT $ join $ fmap runIdentityT (fmap f ma)-}
  {-same as-}
  {-(IdentityT ma) >>= f = IdentityT $ join $ fmap (runIdentityT . f) ma-}
  {-same as                          join and fmap is bind i.e.(>>=) -}
  {-(IdentityT ma) >>= f = IdentityT $ (>>=) ma (runIdentityT . f)-}
  {-same as                          join and fmap is bind i.e.(>>=) -}
  {-(IdentityT ma) >>= f = IdentityT $ ma >>= (runIdentityT . f)-}

instance MonadTrans IdentityT where
  lift = IdentityT

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO
