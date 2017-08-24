{-# LANGUAGE FlexibleInstances #-}
module Functors where

import GHC.Arr

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
     in fmap (*3) changed


{-chapter exercises-}
{-ex3-}
data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

{-ex4-}
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish a) = Truish (f a)
  fmap _ _ = Falsish

{-ex5-}
newtype Mu f = InF { outF :: f (Mu f) }

{-instance Functor Mu where-}
  {-fmap f x = Inf (f x)-}

{-I do not think valid Functor can be written for this-}
     {-because the type kind is (* -> *) -> * instead of * -> *-}

{-ex6-}
data D = D (Array Word Word) Int Int deriving (Eq, Show)

{-same for this-}
{-instance Functor D where-}
  {-fmap f x = undefined-}

{-Reorder the type arguments-}
{-ex1-}
data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b


{-ex2-}
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

{-ex 3-}
data More b a =
    L a b a
  | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

{-writing functor instances for those datatypes-}
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

{-ex2-}
instance Functor (Quant a) where
  fmap f (Bloor x) = Bloor (f x)
  fmap _ (Finance) = Finance
  fmap _ (Desk a) = Desk a

{-ex3-}
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K x) = (K x)

{-ex4-}

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a
  -- should remind you of an instance you've written before
instance Functor (Flip K' a) where
  fmap ff (Flip (K' a)) = Flip (K' (ff a))

{-ex5-}
data EvilGoateeConst b a = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst b


{-ex6-}
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap g (LiftItOut f) = LiftItOut (fmap g f)
  {- usage is fmap (+1) (LiftItOut (Just 1))-}

{-ex7-}
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap ff (DaWrappa f g) = DaWrappa (fmap ff f) (fmap ff g)

{-usage is 'fmap (+1) (DaWrappa (Just 1) (Right 2))'-}

{-ex8-}
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap ff (IgnoringSomething f g) = IgnoringSomething f (fmap ff g)

{-usage is 'fmap (+1) (IgnoringSomething (Right "alabala") (Just 2))'-}

{-ex9-}
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap ff (Notorious x y z) = Notorious x y (fmap ff z)

{-usage is 'fmap (+1) (Notorious (Just 1) (Just 2) (Just 3))'-}

{-ex10-}
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a x) = Cons (f a) (fmap f x)
{-usage is 'fmap (+1) (Cons 1 (Cons 2 (Cons 3 Nil)))'-}

{-ex11-}
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (ffmap x) (ffmap y) (ffmap z)
    where ffmap = fmap f

{-usage is 'fmap (+1) (MoreGoats (OneGoat 1) NoGoat (MoreGoats NoGoat NoGoat (OneGoat 3)))'-}
data TalkToMe a =
    Halt
  | Print String a
  | Read { getFn :: (String -> a)}

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f.g)

{-seems to work but I am not sure how to use it-}
