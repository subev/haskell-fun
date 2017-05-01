module FirstClassFunction where

myNum :: Integer
myNum = 1

myVal f = myNum

bindExp :: Integer -> String
bindExp x = let y = 5 in
                "the integer was: " ++ show x
                ++ " and y was: " ++ show y

bindExp2 :: Integer -> String
bindExp2 x = let y = 5 in
             let z = y + x in
                "the integer was: " ++ show x
                ++ " and y was: " ++ show y
                ++ " and z was: " ++ show z

bindExpShadow :: Integer -> String
bindExpShadow x = let y = 5; x = 25 in
             let z = y + x in
                "the integer was: " ++ show x
                ++ " and y was: " ++ show y
                ++ " and z was: " ++ show z
