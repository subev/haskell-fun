module Queue where

import Criterion.Main

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
  } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x q = Queue ((x:) $ enqueue q) (dequeue q)

refill :: Queue a -> Queue a
refill q = Queue [] ((reverse $ enqueue q) ++ dequeue q)

pop :: Queue a -> Maybe (a, Queue a)
pop q = case (dequeue q) of
          [] -> case (refill q) of
                  (Queue e []) -> Nothing
                  (Queue e (x:rest)) -> Just (x, Queue e rest)

          (x:rest) -> Just (x, Queue (enqueue q) rest)

{-test tools-}

longQueue :: Int -> Queue Int
longQueue i = go i empty
  where go 0 q = q
        go n q = go (n - 1) (push n q)

dequeueTill :: Int -> Queue Int -> Int
dequeueTill x q = case (pop q) of
                         Just (x', q) -> dequeueTill x' q
                         Nothing -> x

pushesAndPops :: Int -> Int
pushesAndPops i = dequeueTill 0 $ longQueue i

{-test cases with array-}

arrayPushesAndPops :: Int -> Int
arrayPushesAndPops i = dequeueArrayTill 0 $ longArray i

{-
  this pop can be generally implemented with
  `Just (last xs, init xs)`
  but performance is still the same
-}
arrayPop :: [Int] -> Maybe (Int, [Int])
arrayPop [] = Nothing
arrayPop (x: []) = Just (x, [])
arrayPop (x: rest) = case (arrayPop rest) of
                       Just (x', rest') -> Just (x', x:rest')

longArray :: Int -> [Int]
longArray 0 = [0]
longArray x = x: longArray(x - 1)

dequeueArrayTill :: Int -> [Int] -> Int
dequeueArrayTill x arr = case (arrayPop arr) of
                         Just (x', arr) -> dequeueArrayTill x' arr
                         Nothing -> x

main :: IO ()
main = defaultMain [
    bench "queue enqueue and dequeue" $ whnf pushesAndPops 1234,
    bench "array enqueue and dequeue" $ whnf arrayPushesAndPops 1234
  ]
