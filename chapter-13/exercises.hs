module Exercises where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower)

clearLine :: String -> String
clearLine =
  (map toLower) . (filter (flip elem ['A'..'z']))

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let cleared = clearLine line1
  case (cleared == reverse cleared) of
    True -> do putStrLn "It's a palindrome!"
               exitSuccess

    False -> putStrLn "Nope!"
