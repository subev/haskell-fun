module VigenereCipher where

import Data.Char
import Control.Monad (forever)

encode :: String -> String -> String
encode m k = zipWith (shiftChar True) m (cycle k)

decode :: String -> String -> String
decode m k = zipWith (shiftChar False) m (cycle k)

shiftChar :: Bool -> Char -> Char -> Char
shiftChar isEncode mchar kchar =
  chr $ newPosition (ord mchar) (offset kchar isEncode)

newPosition :: Int -> Int -> Int
newPosition x delta = (x + delta)

offset :: Char -> Bool -> Int
offset kchar isEncode
  | isEncode = mod (ord kchar - ord 'A') (ord 'z' - ord 'A')
  | otherwise = - (offset kchar True)

main :: IO()
main = forever $ do
  putStr "Message:"
  message <- getLine
  putStr "Secret:"
  key <- getLine
  putStrLn $ "encoded: " ++ encode message key
  putStrLn $ "decoded: " ++ decode (encode message key) key


