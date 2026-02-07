{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int, [Int])
input = getInts >>= \[n, d] -> getInts >>= \as -> return (n, d, as)

output :: Int -> IO ()
output = print

solve :: (Int, Int, [Int]) -> Int
solve (n, d, as) = shakutori 0 d as []
    where
        shakutori i d' (x:y:ys) zs
            | zs == [] = if abs (x - y) >= d' then succ i else i
            | abs (x - y) >= d' = shakutori (succ i) d' (x:ys) (y:zs)
            | otherwise = shakutori i d' (y:ys) (drop 1 zs)

main :: IO ()
main = input >>= output . solve
