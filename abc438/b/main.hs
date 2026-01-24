{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace, digitToInt )
import Data.List ( unfoldr, tails )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, String, String)
input = getInts >>= \[_, m] -> getLine >>= \s -> getLine >>= \t -> return (m, s, t)

output :: Int -> IO ()
output = print

solve :: (Int, String, String) -> Int
solve (m, s, t) = minimum $ map (\u -> distance (take m u) t) $ filter ((>= m) . length) $ tails s

distance :: String -> String -> Int
distance s t = sum $ zipWith (\x y -> (digitToInt x - digitToInt y + 10) `mod` 10) s t

main :: IO ()
main = input >>= output . solve
