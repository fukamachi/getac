module Main where

main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  let
    a = read l1 :: Int
    [b, c] = map (read :: String -> Int) (words l2)
    s = l3
  putStr (show (a + b + c))
  putStr " "
  putStrLn s
