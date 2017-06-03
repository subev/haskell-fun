module TypeConstructors where

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Show, Eq)


{-exercises-}
data Airline = PapuAir
  | Capitaltsr'Us
  | TakeyourchancesUnited
    deriving (Eq, Show)

data Manufactorer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Price = Price Integer
  deriving (Eq, Show)

data Size = Size Int
  deriving (Eq, Show)

data Vehicle = Car Manufactorer Price
  | Plane Airline Size
  deriving (Show, Eq)

myCar = Car Mazda (Price 17000)
urCar = Car Mini (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 5)

{-ex2-}
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane = not . isCar

areCars :: [Vehicle] -> Bool
areCars ((Car _ _): []) = True
areCars ((Car _ _): rest) = areCars rest
areCars _ = False

{-ex3-}
getManu :: Vehicle -> Manufactorer
getManu (Car m _) = m
{-plane is unhandled here-}

data Example = MyExample deriving (Show)
data OtherExample = AnotherExample Integer deriving (Show)
