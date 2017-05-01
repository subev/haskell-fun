module BasedOnType where

i :: a -> a
i = \a -> a

c :: a -> b -> a
c = \x -> \b -> x
{-or could be this-}
{-c a b = a-}

r :: [a] -> [a]
r (first:_) = [first]

r' :: [a] -> [a]
r' array = take 5 array

r'' :: [a] -> [a]
r'' = reverse

{-I believe this is compose, it even starts with co-}
co :: (b -> c) -> (a -> b) -> (a -> c)
co a b = \f -> a (b f)

a :: (a -> c) -> a -> a
a f x = x

{-I would call this apply?-}
a' :: (a -> b) -> a -> b
a' f x = f x
