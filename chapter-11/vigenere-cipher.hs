module VigenereCipher where

import Data.Char

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
main = do
  let message = "This is some message!!! AWESOME zZz"
      key = "crazy secret"
      encoded = encode message key
      decoded = decode encoded key

  putStrLn $ "encoded: " ++ encoded
  putStrLn $ "decoded: " ++ decoded


