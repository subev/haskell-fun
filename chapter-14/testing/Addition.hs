module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello:: IO()
sayHello = putStrLn "hello!!!"

main :: IO()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True

    it "2 + 2 is 4" $ do
      2 + 2 `shouldBe` 4

    it "should divide with remainer" $ do
      (divideBy 5 3) `shouldBe` (1, 2)

    it "15 does not have remainer" $ do
      (divideBy 15 5) `shouldBe` (3, 0)

    it "22 does not have remainer 2 and count 2" $ do
      (divideBy 22 10) `shouldBe` (2, 2)

    it "multiplyBySum works as expected" $ do
      (multiplyBySum 2 7) `shouldBe` 14

    {-PROPERTY TESTING WITH QUICKCHECK-}
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)


divideBy :: Integral a => a -> a -> (a, a)
divideBy num denum = go num denum 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum 0 y = 0
multiplyBySum 1 y = y
multiplyBySum x y = y + multiplyBySum (x - 1) y
