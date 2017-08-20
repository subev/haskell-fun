module Functors2 where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "wohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f1 (f a) -> f1 (f Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [(Maybe a)] -> [(Maybe Char)]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f2 (f1 (f a)) -> f2 (f1 (f Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [a]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)

  putStr "liftedReplace lms: "
  print (liftedReplace lms)

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)

  putStr "twiceLifted lms: "
  print (twiceLifted lms)

  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms)

  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)
