module Phone where

data DaPhone = DaPhone [(Symbols, Digit)]
  deriving (Eq, Show)

type Symbols = String

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

convo :: [String]
convo = ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

phone = DaPhone [
    ( "1", '1' ),
    ( "ABC2", '2' ),
    ( "DEF3", '3' ),
    ( "GHI4", '4' ),
    ( "JKL5", '5' ),
    ( "MNO6", '6' ),
    ( "PQRS7", '7' ),
    ( "TUV8", '8' ),
    ( "WXYZ9", '9' ),
    ( "+ ", '0' ),
    ( ".,", '#' ),
    ( "^", '*' )
  ]

reverseTaps :: DaPhone -> Char -> [(Digit,Presses)]
