module EqInstance where

{-ex 1-}
data TisAnInteger =
  TisAn Integer

{-my answer-}
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

{-ex 2-}
data TwoIntegers =
  Two Integer Integer

{-my answer-}
instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') =
    x == x' || y == y'

{-ex 3-}
data StringOrInt =
  TisAnInt Int | TisAString String

{-my answer-}
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False


{-ex 4-}
data Pair a =
  Pair a a

{-my answer-}
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == y || x' == y'

{-ex 5-}
data Tuple a b = Tuple a b
  deriving Show

{-my answer-}
instance Eq a => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x'
  {-this is redundant-}
  {-(==) _ _ = False-}

  {-for some reason the following example does not work-}
  {-Tuple 1 "123" == Tuple 1 123-}
  {-alright, this happens because of type inference -}
  {-preload the (==) operator with one argument and see its :type-}

{-ex 6-}
data Which a =
  Thisone a | ThatOne a

{-my answer-}
instance Eq (Which a) where
  (==) (Thisone x) (Thisone y) = True
  (==) _ _ = False


{-ex 7-}
data EitherOr a b =
  Hello a | GoodBye b

{-my answer-}
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (GoodBye x) (GoodBye x') = x == x'
  (==) _ _ = False
