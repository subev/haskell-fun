module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO as SIO

-- replace "/usr/share/dict/words" as appropriate
dictWords :: IO String
dictWords = SIO.readFile "/usr/share/dict/words"

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile "/usr/share/dict/words"

main :: IO ()
main = do
  replicateM_ 10 (dictWords >>= print)
  replicateM_ 10 (dictWordsT >>= TIO.putStrLn)
