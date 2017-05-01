module Declrations where

data Rocks = Rocks String
  deriving (Eq, Show, Ord)

data Yeah = Yeah Bool
  deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah
  deriving (Eq, Show, Ord)

phew = Papu (Rocks "chases") (Yeah True)

equallityForAll :: Papu -> Papu -> Bool
equallityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Ordering
comparePapus p p' = compare p p'
