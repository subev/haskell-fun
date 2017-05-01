
module TypeChecks where

{-ex 1-}
data Person = Person Bool
  deriving Show

{-instance Show Person where-}
{-???how to access the bool-}
  {-show person = "PerSoN"-}

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

{-use it with-}
{-printPerson $ Person True-}

{-ex 2-}
data Mood = Blah | Woot deriving (Show{-, Eq-})

instance Eq Mood where
  (==) Blah Woot = False
  (==) _ _ = True

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

{-ex 4-}
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool" "pff"
s2 = Sentence "Julie" "loves" "dogs"
s3 = Sentence "Julie" "loves" "dogs"


