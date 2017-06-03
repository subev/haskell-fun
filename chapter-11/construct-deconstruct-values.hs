module ConstructAndDeconstructValues where

data GuessWhat =
  ChichenButt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
      First a
    | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)

{-examples-}
{-1-}
newtype NumCow =
  NumCow Int deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type FarmHouse' = Product NumCow NumPig

{-2-}
newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmHouse = BigFarmHouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmHouse' =
  Product NumCow (Product NumPig NumSheep)

{-3-}
type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

{-constructing-}

idIdentity :: Id ( a -> a)
idIdentity = MkId $ \x -> x

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

{-constructing record syntax-}
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord2 :: RecordProduct Integer Float
myRecord2 = RecordProduct { pfirst = 42
                          , psecond = 0.0001 }

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem,
              language :: ProgrammingLanguage }
              deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, language = Haskell }

feelingWizard :: Programmer
feelingWizard = Programmer { os = GnuPlusLinux, language = Agda }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux , OpenBSDPlusNevermindJustBSDStill , Mac , Windows ]

allProgrammingLanguanges :: [ProgrammingLanguage]
allProgrammingLanguanges = [ Haskell , Agda , Idris , PureScript ]

allPrgrms = [Programmer x y | x <- allOperatingSystems, y <- allProgrammingLanguanges]

{-deconstructing-}
newtype FarmerName = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer = Farmer FarmerName Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec {name :: Name,
                            acres :: Acres,
                            farmerType :: FarmerType} deriving Show

isDairyFarmer2 :: FarmerRec -> Bool
isDairyFarmer2 farmer = case farmerType farmer of
                          DairyFarmer -> True
                          _ -> False
