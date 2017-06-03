module NoInfixList where

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

froo = Cons 1 (Cons 2 (Cons 3 Nil))
