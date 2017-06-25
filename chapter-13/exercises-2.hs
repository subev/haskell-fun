module Exercises2 where

type Name = String
type Age = Integer

data Person = Person Name Age
      deriving (Eq, Show)

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                 deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- getLine
  let ageAsNumber = read age :: Integer
  let person = mkPerson name ageAsNumber
  case person of
    (Left invalidPerson) -> putStrLn $ show invalidPerson
    (Right person) -> putStrLn $ "Yay successfully got a person: "
                    ++ (show person)
