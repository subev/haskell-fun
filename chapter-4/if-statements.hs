module IfStatements where

coolCheck :: String -> IO()
coolCheck coolnes = do
  if cool coolnes
     then putStrLn "eyyy you are cool"
  else
    putStrLn "Not cool enough!"
  where
    cool v = v == "y0"
