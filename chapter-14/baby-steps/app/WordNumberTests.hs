module WordNumberTests where

import WordNumber (digitToWord, wordNumber, digits)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "retuns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "retuns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1,2,3] for 123" $ do
      digits 123 `shouldBe` [1, 2, 3]

  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-one-one for 911" $ do
      wordNumber 911 `shouldBe` "nine-one-one"

