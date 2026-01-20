{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int, Int, Int)
input = getInts >>= \[p, q] -> getInts >>= \[x, y] -> return (p, q, x, y)

output :: String -> IO ()
output = putStrLn

solve :: (Int, Int, Int, Int) -> String
solve (p, q, x, y) = if p <= x && x < p + 100 && q <= y && y < q + 100 then "Yes" else "No"

main :: IO ()
main = input >>= output . solve