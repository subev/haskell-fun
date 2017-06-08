module PersonValidating where

{-model-}
type Name = String
type Age = Int

data Person = Person Name Age deriving Show


{-invalid-}
type ValidatePerson a = Either [PersonInvalid] a

data PersonInvalid = NameEmpty | AgeTooLow
                   deriving (Eq)

toString :: PersonInvalid -> String
toString NameEmpty = "Name Empty"
toString AgeTooLow = "Age is too low"

instance Show PersonInvalid where
  show = toString


{-validating fns-}
ageOk :: Age -> Either [PersonInvalid] Age
ageOk age = case age >= 0 of
              True -> Right age
              False -> Left [AgeTooLow]

nameOk :: Name -> Either [PersonInvalid] Name
nameOk name = case name /= "" of
                True -> Right name
                False -> Left [NameEmpty]

{-constructors-}
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
 mkPerson' (nameOk name) (ageOk age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age ->
  ValidatePerson Person
mkPerson' (Right validName) (Right validAge) =
  Right (Person validName validAge)

mkPerson' (Left badName) (Left badAge) =
  Left (badName ++ badAge)

mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge
