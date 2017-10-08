module WhatHappens where

import Control.Concurrent
import Debug.Trace

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero


blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

main2 :: IO ()
main2 = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w
