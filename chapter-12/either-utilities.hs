module EitherUtilities where

{-small utility functions for either -}

{-ex1-}
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' all = foldr (\x -> \arr ->
  case x of
    Left a -> a: arr
    _ -> arr
                   ) [] all

{-ex2-}
rights' :: [Either a b] -> [b]
rights' [] = []
rights' all = foldr (\x -> \arr ->
  case x of
    Right a -> a: arr
    _ -> arr
                   ) [] all

{-ex3-}
partitionsEither :: [Either a b] -> ([a], [b])
partitionsEither xs = (lefts' xs, rights' xs)

{-ex4-}
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

{-ex5-}
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

{-ex6-}
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\r -> Just (f r))

