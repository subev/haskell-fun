module Cow where

import Control.Applicative (liftA3)

data Cow = Cow {
  name :: String
  ,age :: Int
  ,weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative x
  | x < 0 = Nothing
  | otherwise = Just x

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
         then Nothing
         else Just c

mkSpecialCow :: String -> Int -> Int -> Maybe Cow
mkSpecialCow n a w =
  case noEmpty n of
    Nothing -> Nothing
    Just validatedName ->
      case noNegative a of
        Nothing -> Nothing
        Just validatedAge ->
          case noNegative w of
            Nothing -> Nothing
            Just validatedWeight ->
              weightCheck (Cow validatedName validatedAge validatedWeight)

{-monad time!-}

makeTheCowLikeAPro :: String -> Int -> Int -> Maybe Cow
makeTheCowLikeAPro n a w = do
  name <- noEmpty n
  age <- noNegative a
  weight <- noNegative w
  weightCheck (Cow name age weight)


makeTheCowLikeAPro'' :: String -> Int -> Int -> Maybe Cow
makeTheCowLikeAPro'' n a w = do
  noEmpty n >>=
    \name -> noNegative a >>=
      \age -> noNegative w >>=
        \weight -> weightCheck (Cow name age weight)

{-this is the case using applicative, since weightCheck is applied to a Maybe you end up with one more maybe-}
{-this is because when lifting the constructor you end up with a Maybe and you cannot get rid of it-}
makeTheCoww :: String -> Int -> Int -> Maybe (Maybe Cow)
makeTheCoww n a w =
  weightCheck <$> liftA3 Cow (noEmpty n) (noNegative a) (noNegative w)

