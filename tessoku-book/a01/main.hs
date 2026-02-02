module Main where

input :: IO Int
input = readLn

output :: Int -> IO ()
output = print

solve :: Int -> Int
solve n = n * n

main :: IO ()
main = input >>= output . solve