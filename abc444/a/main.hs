module Main where

input :: IO Int
input = readLn >>= return 

output :: String -> IO ()
output = putStrLn

solve :: Int -> String
solve n = if n `mod` 111 == 0 then "Yes" else "No"

main :: IO ()
main = input >>= output . solve
