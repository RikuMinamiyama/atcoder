{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [String])
input = getInts >>= \[h, w] -> replicateM h getLine >>= \grid -> return (w, grid)

output :: Int -> IO ()
output = print

solve :: (Int, [String]) -> Int
solve (w, grid) = last row
    where
        row = foldl step (0 : 1 : replicate (pred w) 0) grid
        step prev s = scanl go 0 (zip s (drop 1 prev))
            where
                go left (c, above)
                    | c == '.' = (left + above) `mod` 1000000007
                    | c == '#' = 0
                    | otherwise = undefined

main :: IO ()
main = input >>= output . solve
