module Print3Broken where

{-printSecond :: String-}
{-printSecond = do-}
  {-putStrLn greeting-}

main :: IO ()
main = do
  greet "world"
  where
    greet :: String -> IO ()
    greet x = do {-this marks a block-}
      putStrLn result
      where
        prepend :: String -> String
        prepend = (++x)
        result :: String
        result = prepend "hello "

