module Main where

import Data.List ( unfoldr )

input :: IO Int
input = readLn

output :: String -> IO ()
output = putStrLn

solve :: Int -> String
solve = padLeft 10 '0' . reverse . concatMap show . unfoldr go
    where
        go k
            | k == 0 = Nothing
            | otherwise = Just (k `mod` 2, k `div` 2)

padLeft :: Int -> Char -> String -> String
padLeft n c str = replicate (n - length str) c ++ str

main :: IO ()
main = input >>= output . solve
