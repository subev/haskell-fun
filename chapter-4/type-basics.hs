module TypeBasics where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah






{-we can have edge cases covered like this-}
crazy :: Int -> Int
crazy 5 = 3;
crazy x = x + 5;
