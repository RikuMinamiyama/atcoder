{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, sortBy )
import Data.Ord ( comparing )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [Int])
input = readLn >>= \n -> getInts >>= \ts -> return (n, ts)

output :: [(Int, Int)] -> IO ()
output = putStrLn . unwords . map (show . fst)

solve :: (Int, [Int]) -> [(Int, Int)]
solve (x, ts) = take 3 . sortBy (comparing snd) . zip [1 .. ] $ ts

main :: IO ()
main = input >>= output . solve