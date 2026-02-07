{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int)
input = getInts >>= \[n, k] -> return (n, k)

output :: Int -> IO ()
output = print

solve :: (Int, Int) -> Int
solve (n, k) = foldl go 0 [1..n]
    where
        go acc m
            | digitSum m == k = succ acc
            | otherwise = acc
        digitSum 0 = 0
        digitSum x = (x `mod` 10) + digitSum (x `div` 10)

main :: IO ()
main = input >>= output . solve
