module Foo where

import Control.Monad (join, liftM, liftM2)
import Control.Applicative (liftA, (*>))

bind :: Monad m => (a -> m b) -> m a -> m b
bind m g = join $ fmap m g

{-usage is-}
result1 = (\x -> Just (x + 2)) `bind` (Just 3)
result2 = (Just 3) >>= (\x -> Just (x + 2))

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"


binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = do
  getLine >>= putStrLn


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y hello thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>= (\name -> putStrLn ("y helo thar: " ++ name))

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
    ++ name ++ " who is: "
    ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>= (\name ->
    putStrLn "age pls:" >>
    getLine >>= (\age ->
      putStrLn ("y helo thar: " ++ name ++ "and your age is " ++ age)))

{-example with lists-}
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  y <- xs
  if even x
     then [111111,x]
     else [222222,y]
