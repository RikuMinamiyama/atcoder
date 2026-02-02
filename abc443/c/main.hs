{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [Int])
input = getInts >>= \[_, t] -> getInts >>= \as -> return (t, as)

output :: Int -> IO ()
output = print

solve :: (Int, [Int]) -> Int
solve (t, as)
    | as == [] = t
    | otherwise = let (s, j) = foldl step (0, 0) as in s + max 0 (t - j)
    where
        step (sum', j) x
            | x < j = (sum', j)
            | otherwise  = (sum' + x - j, x + 100)

main :: IO ()
main = input >>= output . solve
