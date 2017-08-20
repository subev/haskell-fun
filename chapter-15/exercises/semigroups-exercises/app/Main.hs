module Main where

import Data.Semigroup
import Test.QuickCheck

{-types-}
data Trivial = Trivial
  deriving (Eq, Show)

newtype Identity a = Identity a
  deriving (Eq, Show)

data Two a b = Two a b
  deriving (Eq, Show)

data Three a b c = Three a b c
  deriving (Eq, Show)

data Four a b c d = Four a b c d
  deriving (Eq, Show)

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

newtype Comp a = Comp { unComp :: (a -> a) }

data Validation a b =
  Failure' a | Success' b deriving (Eq, Show)

{-newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)-}

{- instances of Semigroup -}

instance Semigroup Trivial where
  (<>) Trivial Trivial = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Semigroup (Or a b) where
  x@(Snd _) <> _ = x
  _ <> x = x

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure' x) (Failure' y) = Failure' (x <> y)
  (<>) x@(Failure' _) _ = x
  (<>) _ x@(Failure' _) = x
  {-not sure what to do when there is no failture, will just pick first-}
  (<>) x _ = x

{-instance Semigroup b => Semigroup (AccumulateRight a b) where-}

  {-(AccumulateRight (Success' x)) <> (AccumulateRight (Success' y)) = AccumulateRight (Success' (x <> y))-}
  {-_ <> z@(AccumulateRight (Failure' y)) = z-}
  {-z <> _ = z-}

{-instances of Monoid-}

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj x) (BoolConj y) = BoolConj (x && y)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\x -> mempty)
  mappend (Combine f) (Combine g) = Combine (\x -> mappend (f x) (g x))

instance Monoid a => Monoid (Comp a) where
  mempty = Comp id
  mappend (Comp f) (Comp g) = Comp (f . g)



{-instances of Arbitrary-}
instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a), (1, return $ Snd b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure' a), (1, return $ Success' b)]

{-instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight (Validation a b)) where-}
  {-arbitrary = do-}
    {-a <- arbitrary-}
    {-b <- arbitrary-}
    {-frequency [(1, return $ Validation (Failure' a)), (1, return $ Validation (Success' b))]-}


{-the test comfirming the law of associativity-}
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity x = (x <> mempty) == x

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity x = (mempty <> x) == x

{-type aliases for tests-}
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = (Identity String) ->
                     (Identity String) ->
                     (Identity String) -> Bool

type TwoAssoc = (Two String String) ->
                (Two String String) ->
                (Two String String) -> Bool

type ThreeAssoc = (Three String String String) ->
                (Three String String String) ->
                (Three String String String) -> Bool

type FourAssoc = (Four String String String String) ->
                (Four String String String String) ->
                (Four String String String String) -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = (Or String String) ->
               (Or String String) ->
               (Or String String) -> Bool

type ValidationAssoc = (Validation String String) ->
                       (Validation String String) ->
                       (Validation String String) -> Bool


{-ex 8-}
newtype Mem s a = Mem {
  runMem :: s -> (a, s)
}

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
  mappend (Mem f) (Mem g) = Mem $ \x -> (mappend (fst (f x)) (fst (g x)), snd (g ( snd ( f x ) )))

instance (Monoid a, Semigroup a) => Semigroup (Mem s a) where
  (<>) = mappend

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: (Identity String) -> Bool)
  quickCheck (monoidRightIdentity :: (Identity String) -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: (Two String String) -> Bool)
  quickCheck (monoidRightIdentity :: (Two String String) -> Bool)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)

  print "last exercises (8)"
  print $ runMem (f' <> mempty) 0 {- ("hi",1) -}
  print $ runMem (mempty <> f') 0 {- ("hi",1) -}
  print $ (runMem mempty 0 :: (String, Int)) {- ("", 0) -}
  print $ runMem (f' <> mempty) 0 == runMem f' 0 {- True -}
  print $ runMem (mempty <> f') 0 == runMem f' 0 {- True -}
