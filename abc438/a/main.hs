{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int)
input = getInts >>= \[d, f] -> return (d, f)

output :: Int -> IO ()
output = print

solve :: (Int, Int) -> Int
solve (d, f) = 7 - (d - f) `mod` 7

main :: IO ()
main = input >>= output . solve
