{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.Array ( accumArray, assocs )

input :: IO Int
input = readLn

output :: [Int] -> IO ()
output xs = print (length xs) >> putStrLn (unwords $ map show xs)

solve :: Int -> [Int]
solve n = map fst
        $ filter ((== 1) . snd)
        $ assocs
        $ accumArray (+) 0 (1, n)
        $ [(x*x + y*y, 1) | x <- [1..isqrt n], y <- [succ x..isqrt (n-x*x)]]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main :: IO ()
main = input >>= output . solve
