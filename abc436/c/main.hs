{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )
import Data.Set qualified as Set

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO [(Int, Int)]
input = getInts >>= \[_, m] -> replicateM m (getInts >>= \[r, c] -> return (r, c)) >>= \rcs -> return rcs

output :: Int -> IO ()
output = print

solve :: [(Int, Int)] -> Int
solve = flip div 4 . Set.size . foldl phi Set.empty
    where
        phi s (r, c) = if any (`Set.member` s) xs then s else Set.union s (Set.fromList xs)
            where xs = [(r, c), (succ r, c), (r, succ c), (succ r, succ c)]

main :: IO ()
main = input >>= output . solve
