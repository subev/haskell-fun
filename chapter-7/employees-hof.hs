module TechCompany where

data Employee = Coder
                | Manager
                | Veep
                | CEO
                deriving (Eq, Show, Ord)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'


codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
  -> Employee
  -> Employee
  -> IO()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither is the boss"
    LT -> (flip reportBoss) e e'

{-other exercises-}

dodgy :: Num a => a -> a -> a
dodgy x y = x + 10 * y

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

{-answers-}
ex2 = dodgy 1 1       {-11-}
ex3 = dodgy 2 2       {-22-}
ex4 = dodgy 1 2       {-21-}
ex5 = dodgy 2 1       {-12-}
ex6 = oneIsOne 1      {-11-}
ex7 = oneIsOne 2      {-21-}
ex8 = oneIsTwo 1      {-21-}
ex9 = oneIsTwo 2      {-22-}
ex10 = oneIsOne 3     {-31-}
ex11 = oneIsTwo 3     {-23-}
