module Main where

input :: IO String
input = getLine

output :: String -> IO ()
output str = putStr str >> putStrLn "s"

main :: IO ()
main = input >>= output
