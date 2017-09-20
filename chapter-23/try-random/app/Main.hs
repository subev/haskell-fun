{-# LANGUAGE InstanceSigs #-}
module Main where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie x
  |x == 1 = DieOne
  |x == 2 = DieTwo
  |x == 3 = DieThree
  |x == 4 = DieFour
  |x == 5 = DieFive
  |x == 6 = DieSix
  |otherwise = error $ "intToDie got non 1-6 integer: " ++ show x

main :: IO ()
main = undefined

rollDiceThreeTimes :: (Die, Die, Die)
rollDiceThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR(1, 6) gen
             in go (sum + die) (count + 1) nextGen

rollsToGenN :: Int -> StdGen -> Int
rollsToGenN x g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= x = count
          | otherwise =
            let (die, nextGen) = randomR(1, 6) gen
             in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged x g = go 0 (0, []) g
  where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum (count, log) gen
          | sum >= x = (count, log)
          | otherwise =
            let (die, nextGen) = randomR(1, 6) gen
             in go (sum + die) (count + 1, ((intToDie die): log)) nextGen



{-exercises implementing the State Monad-}
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x ->
    let (a, s) = g x
     in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \x ->
    let (a, _) = g x
        (f', _) = f x
     in (f' a, x)

{-usage is-}
applicativeTest = runMoi ( (Moi $ \s -> ((+7), s)) <*> (Moi $ \s -> (13, s)) ) 5


instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
          -> (a -> Moi s b)
          -> Moi s b
  (Moi f) >>= g = Moi $ \x ->
    let (a, _)   = f x
        Moi (gb) = g a
     in gb x
