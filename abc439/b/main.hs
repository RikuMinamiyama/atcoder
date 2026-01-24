{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.List ( unfoldr )

input :: IO Int
input = readLn

output :: String -> IO ()
output = putStrLn

-- 十分な回数を繰り返して1になれば良い
solve :: Int -> String
solve n = if iterate (f . g) n !! 1000 == 1 then "Yes" else "No"
    where
        f :: [Int] -> Int
        f = foldl (flip ((+) . (^2))) 0

        g :: Int -> [Int]
        g = unfoldr (\k -> if k == 0 then Nothing else Just (k `mod` 10, k `div` 10))

main :: IO ()
main = input >>= output . solve
