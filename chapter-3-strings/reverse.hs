module Reverse where

main :: IO ()
main = do
  print (rvrsSevenLetterWord "Awesome")
  print $ rvrsSevenLetWord "Awesome"

take5 :: String -> String
take5 xs = drop 5 xs

{-at5 :: [a] -> a-}
at5 :: String -> Char
at5 xs = (!!) xs 5

scream :: String -> String
scream = \s -> s ++ "!"

helloAt :: Int -> Char
helloAt i = "Lol this is amazing!" !! i

{-this is the ugliest thing I've ever written-}
rvrsSevenLetterWord :: String -> String
rvrsSevenLetterWord str = concat [
    str !! 6 : "",
    str !! 5 : "",
    str !! 4 : "",
    str !! 3 : "",
    str !! 2 : "",
    str !! 1 : "",
    str !! 0 : ""
  ]

rvrsSevenLetWord :: String -> String
rvrsSevenLetWord str = concat [
    take 1 $ drop 6 str,
    take 1 $ drop 5 str,
    take 1 $ drop 4 str,
    take 1 $ drop 3 str,
    take 1 $ drop 2 str,
    take 1 $ drop 1 str,
    take 1 str
  ]
