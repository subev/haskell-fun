module Options where

import Data.Monoid

data Optional a = Only a | Nada deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mappend Nada Nada = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only (x <> y)

  mempty = Nada
