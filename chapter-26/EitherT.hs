{-# LANGUAGE InstanceSigs #-}
module EitherT where

import Control.Applicative (liftA2)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mma) = EitherT $ (fmap . fmap) f mma

instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ pure $ pure x
  (EitherT mmab) <*> (EitherT mma) = EitherT $ (liftA2 . liftA2) ($) mmab mma
  {-(EitherT mmab) <*> (EitherT mma) = EitherT $ ((<*>) <$> mmab) <*> mma-}

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mma) >>= f = EitherT $ do
    ma <- mma
    case ma of
      (Left e) -> pure (Left e)
      (Right a) -> runEitherT (f a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mma) = EitherT $ do
  fmap (\ma -> case ma of
                 (Left e) -> (Right e)
                 (Right a) -> (Left a)
       ) mma

eitherT :: Monad m =>
  (a -> m c)
  -> (b -> m c)
  -> EitherT a m b
  -> m c
eitherT famc fbmc x@(EitherT amb) = do
  ab <- amb
  case ab of
    (Left a) -> famc a
    (Right b) -> fbmc b
