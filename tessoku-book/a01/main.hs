{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

input :: IO Int
input = readLn >>= \n -> return n

output :: Int -> IO ()
output = print

solve :: Int -> Int
solve n = n * n

main :: IO ()
main = input >>= output . solve