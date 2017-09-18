{-# LANGUAGE InstanceSigs #-}

module ReaderPlayground where

import Control.Applicative
import Data.Char

hurr = (*2)

durr = (+10)

m :: Integer -> Integer
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

{-in the following cases 1 arguments is feeded to both hurr and durr and the result is-}
m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

foo = ((/) <$> (+ 990)) <*> (+ 90)

{-example1-}

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = reverse . cap

fmapped :: [Char] -> [Char]
fmapped = fmap reverse cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

monadic :: [Char] -> ([Char], [Char])
monadic = do
  uppered <- cap
  revved <- rev
  return (uppered, revved)

monadicWithBind :: [Char] -> ([Char], [Char])
{-TODO dunno how to do it with >>= operator-}
monadicWithBind = cap >>= (\u -> (rev >>= (\r -> return (u, r))))

{-example-}

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
        humanName :: HumanName
      , dogName   :: DogName
      , address   :: Address
}

data Dog = Dog {
        dogsName    :: DogName
      , dodsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person
        (HumanName "Big Bird")
        (DogName "Barkley")
        (Address "Sesame Street")

chris :: Person
chris = Person
          (HumanName "Chris Allen")
          (DogName "Papu")
          (Address "Austin")


{-writing the above without reader-}
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

{-the reader custom type-}
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

{-ex1-}
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = (fmap f a) <*> b

{-ex2-}
asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader $ \_ -> x

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

getDogR2 :: Reader Person Dog
getDogR2 = Reader (Dog <$> dogName <*> address)

{-MONAD-}

baz :: (Functor f, Num a) => f a -> f a
baz r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

{-INSTANCE READER MONAD-}
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  {-this is crazy-}
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

{-TODO with my Reader-}

{-getDogRM2 :: Reader Person Dog-}
{-getDogRM2 =-}
  {-return $ dogName >>= (\name -> (address >>= (\addy -> return $ Dog name addy)))-}

{-getDogRM3 :: Reader Person Dog-}
{-getDogRM3 = do-}
  {-dog <- dogName-}
  {-adr <- address-}
  {-return $ Dog dog adr-}
