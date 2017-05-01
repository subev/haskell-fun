module TypeKwonDo where


f :: Int -> String
f index = ["Few", "Strings", "here"] !! index

g :: String -> Char
g str = head str

h :: Int -> Char
h index = "Some string again huh?" !! index

{-ex2-}

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e someA = w (q someA)

{-ex3-}
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X,Y) -> (Z, Z)
xform tupleXY = (xz $ fst tupleXY,yz $ snd tupleXY)
{-or apply the yz two times, or swap them ...-}
{-xform tupleXY = (xz $ fst tupleXY,xz $ fst tupleXY)-}

{-ex4-}
mungle :: (x -> y) -> (y -> (w,z)) -> x -> w
mungle a1 a2 a3 = fst $ a2 $ a1 a3
