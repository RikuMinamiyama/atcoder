{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )
import Data.Graph ( buildG )
import Data.Array ( listArray, elems, (!) )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [[Int]])
input = getInts >>= \[n, m] -> replicateM m getInts >>= \xys -> return (n, xys)

output :: Int -> IO ()
output = print

solve :: (Int, [[Int]]) -> Int
solve (n, xys) = maximum (elems dp)
    where
        graph = buildG (1, n) [(x, y) | [x, y] <- xys]
        dp = listArray (1, n) [f v | v <- [1..n]]
        f v = case graph ! v of
            [] -> 0
            us -> maximum [dp ! u + 1 | u <- us]

main :: IO ()
main = input >>= output . solve
