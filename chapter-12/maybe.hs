module PlayingMaybe where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n
     then Just (n + 2)
     else Nothing

type Name = String
type Age = Int

data Person = Person Name Age deriving Show

{-smart contructor, when it validates-}
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
 | name /= "" && age >= 0 = Just $ Person name age
 | otherwise = Nothing

{-using Either-}

data PersonInvalid = NameEmpty | AgeTooLow
                   deriving (Eq)

toString :: PersonInvalid -> String
toString NameEmpty = "Name Empty"
toString AgeTooLow = "Age is too low"

instance Show PersonInvalid where
  show = toString

mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

