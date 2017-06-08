module MaybeUtilities where

{-small utility functions for maybe-}
{-1-}
myIsJust :: Maybe a -> Bool
myIsJust (Just _) = True
myIsJust _ = False

myIsNothing :: Maybe a -> Bool
myIsNothing = not . myIsJust

{-2-}
mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe def f Nothing = def
mayybe def f (Just x) = f x

{-3-}
myFromMaybe :: a -> Maybe a -> a
myFromMaybe def Nothing = def
myFromMaybe def (Just x) = x

{-4-}
myListToMaybe :: [a] -> Maybe a
myListToMaybe [] = Nothing
myListToMaybe (x:_) = Just x

{-5-}
myMaybeToList :: Maybe a -> [a]
myMaybeToList Nothing = []
myMaybeToList (Just x) = [x]

{-6-}
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes [] = []
myCatMaybes (Nothing: rest) = myCatMaybes rest
myCatMaybes (Just x: rest) = x: (myCatMaybes rest)

{-7 this is 'sequence'-}
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing: _) = Nothing
flipMaybe ((Just x): rest) = mayybe Nothing (\xs -> Just (x: xs)) (flipMaybe rest)


