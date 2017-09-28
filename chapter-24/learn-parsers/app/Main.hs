module Main where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop


oneThenEof = one >> eof

two = one >> char '2'
twoThenEof = two >> eof

oneS :: Parser String
oneS = string "1"

pNL s = putStrLn ('\n' : s)

oneTwoThreeAndEof :: Parser ()
oneTwoThreeAndEof = (string "123") >> eof


testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
