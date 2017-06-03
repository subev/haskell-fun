module SumTypes where

import  Data.Int

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumba = Numba (-127)

data Person = MkPerson String Int deriving (Eq, Show)
neim (MkPerson a _) = a

{-record syntax-}
data Personn = MkPersonn { name :: String, age :: Int }
  deriving (Eq, Show)

neimm (MkPersonn a _) = a

