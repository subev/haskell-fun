{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

import Control.Monad.IO.Class

data Config = Config {
  counts :: IORef (M.Map Text Integer)
, prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
  -> M.Map Text Integer
  -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k new m, new)
  where new = fromMaybe 0 (M.lookup k m) + 1

app :: Scotty ()
app = get "/:key" $ do
  config <- lift ask
  unprefixed <- param "key"
  let key' = mappend (prefix config) unprefixed
      ior = (counts config)
      cmap = readIORef ior
  (cmap', newI) <- liftIO $ bumpBoomp key' <$> cmap
  liftIO $ writeIORef ior cmap'
  html $ mconcat [ "<h1>Success! Count was: " , TL.pack $ show newI , "</h1>" ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR r = runReaderT r $ config
  scottyT 3000 runR app
