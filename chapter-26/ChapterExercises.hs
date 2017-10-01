module ChapterExercises where

import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> return (r - 1)

rDec' :: Num a => Reader a a
rDec' = reader (flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn $ "Hi: " ++ show r
  return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " ++ show s
  return (show s, s + 1)

