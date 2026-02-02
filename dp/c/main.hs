{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, foldl' )
import Control.Monad ( replicateM )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO [[Int]]
input = readLn >>= \n -> replicateM n getInts >>= \abcs -> return abcs

output :: Int -> IO ()
output = print

solve :: [[Int]] -> Int
solve abcs = maximum [dpA, dpB, dpC]
    where
        (dpA, dpB, dpC) = foldl' step (0, 0, 0) . map toTriple $ abcs
            where
                step (s, t, u) (a, b, c) = (max t u + a, max u s + b, max s t + c)

toTriple :: [Int] -> (Int, Int, Int)
toTriple (a:b:c:[]) = (a, b, c)
toTriple _ = undefined

main :: IO ()
main = input >>= output . solve
