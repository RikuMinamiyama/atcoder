module Main where

input :: IO String
input = getLine

output :: Int -> IO ()
output = print

solve :: String -> Int
solve = length . filter (`elem` "ij")

main :: IO ()
main = input >>= output . solve
