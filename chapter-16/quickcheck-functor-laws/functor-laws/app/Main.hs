module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g (fmap f x))

li x = functorCompose (+1) (*2) (x :: [Int])

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck li

  {-below is Function generation-}
  quickCheck (functorCompose' :: IntFC)

  {-exercises-}

  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Two Int String)
  quickCheck (functorCompose' :: Two Int String-> Fun String String -> Fun String String -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Three Int String Char)
  quickCheck (functorCompose' :: Three Int String Char -> Fun Char Char -> Fun Char Char -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Three' Int Char)
  quickCheck (functorCompose' :: Three' Int Char -> Fun Char Char -> Fun Char Char -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Four Int String Bool Char)
  quickCheck (functorCompose' :: Four Int String Bool Char -> Fun Char Char -> Fun Char Char -> Bool)

  quickCheck $ \x -> functorIdentity (x :: Four' Int Char)
  quickCheck (functorCompose' :: Four' Int Char -> Fun Char Char -> Fun Char Char -> Bool)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return (Four' a a' a'' b)


{-exercises implementing instance of Functor-}

{-ex1-}
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

{-ex2-}
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

{-ex3-}
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x b) = Two x (f b)

{-ex4-}
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y c) = Three x y (f c)

{-ex5-}
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

{-ex6-}
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

{-ex7-}
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

{-Exercise Possibly-}
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers x) = Yeppers (f x)
  fmap _ _ = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second (f x)
  fmap _ (First x) = First x


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)


{-this implementation is only possible because Constant has two type arguments-}
{-if there was no 'b' in the above case,
     the compiler will requre us to modify the v variable below-}

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v
