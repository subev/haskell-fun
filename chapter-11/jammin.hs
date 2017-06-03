module Jammin where
import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)
{-data JamJar = Fruit Int deriving (Eq, Show)-}

{-record syntax-}

data JamJar = Jam { fruit :: Fruit, quantity :: Int }
  deriving (Eq, Show, Ord)

row1 = Jam Apple 11
row2 = Jam Peach 22
row3 = Jam Plum 322
row4 = Jam Apple 42
row5 = Jam Blackberry 52
row6 = Jam Peach 9
row7 = Jam Plum 15

allJam = [row1, row2, row3, row4, row5, row6, row7]

allQuantity = sum . map quantity

mostRow :: [JamJar] -> JamJar
mostRow (x:[]) = x
mostRow (x:xs) = if quantity x > quantity (mostRow xs)
                then x
                else (mostRow xs)


sortJars :: [JamJar] -> [JamJar]
sortJars = sortBy (compareKind)

compareKind :: JamJar -> JamJar -> Ordering
compareKind a b = compare (fruit a) (fruit b)

groupJam :: [JamJar] -> [[JamJar]]
groupJam  = groupBy isSameFruit . sortJars

isSameFruit :: JamJar -> JamJar -> Bool
isSameFruit a b = (fruit a) == (fruit b)
