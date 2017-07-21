module Tests where

import Test.QuickCheck
import Hangman

prop_sameLength :: Puzzle -> Bool
prop_sameLength puzzle@(Puzzle word filledSoFar (GuessedSoFar s tries)) =
  let (Puzzle newWord _) = fillInCharacter puzzle
   in length word == length newWord


main :: IO ()
main = quickCheck prop_sameLength
