{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)


liftReaderT :: (Monad m) => m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

liftStateT :: (Monad m) => m a -> StateT s m a
liftStateT m = StateT (\s -> do
  a <- m
  return (a, s))


main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT . lift . lift . lift) $ putStrLn "hello"
    {-line above same as those below-}
    (ActionT
      . (ExceptT . fmap Right)
      . liftReaderT
      . liftStateT) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

