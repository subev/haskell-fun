module RegisteredUser1 where

newtype Username = Username String
  {-deriving Show-}
newtype AccountNumber = AccountNumber Integer
  {-deriving Show-}

data User = UnregisteredUser |
  RegisteredUser Username AccountNumber
  {-deriving Show-}

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username uname)
            (AccountNumber number))
          = putStrLn $ uname ++ " " ++ show number

data WherePenguindsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguindsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguindsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguindsLive
gimmeWhereTheyLive (Peng p) = p

humboldt :: Penguin
humboldt = Peng SouthAmerica
gentoo :: Penguin
gentoo = Peng Antarctica
macaroni :: Penguin
macaroni = Peng Antarctica
little :: Penguin
little = Peng Australia
galapagos :: Penguin
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

{-Tupples-}
f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))
