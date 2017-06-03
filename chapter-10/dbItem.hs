module DbItem where

import Data.Time

data DbItem = DbString String
            | DbNumber Integer
            | DbDate UTCTime
            deriving (Eq, Ord, Show)

theDB :: [DbItem]
theDB = [
        DbDate (UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
        ),
        DbNumber 9001,
        DbString "Hello World!",
        DbDate (UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
        ),
        DbNumber 3
      ]

ex1FilterDB :: [DbItem] -> [UTCTime]
ex1FilterDB = foldr (\x acc ->
    case x of
      DbDate x -> x:acc
      _ -> acc
  ) []


ex2FilterDB :: [DbItem] -> [Integer]
ex2FilterDB = foldr (\x acc ->
    case x of
      DbNumber x -> x:acc
      _ -> acc
  ) []

ex3MostRecent :: [DbItem] -> UTCTime
ex3MostRecent = foldr (\x y ->
      case x of
        DbDate x -> max x y
        _ -> y
  ) (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))

ex4SumAllInt :: [DbItem] -> Integer
ex4SumAllInt = foldr (\x y ->
      case x of
        DbNumber x -> x + y
        _ -> y
  ) 0

ex4SumAllInt :: [DbItem] -> Double
ex4SumAllInt = foldr (\x y ->
      case x of
        DbNumber x -> x + y
        _ -> y
  ) 0
