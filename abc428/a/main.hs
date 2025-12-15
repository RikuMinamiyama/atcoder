module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
    [s, a, b, x] <- getInts
    print $ solve s a b x

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

solve :: Int -> Int -> Int -> Int -> Int
solve s a b x
    | a <= x = s * a + solve s a b (x - a - b)
    | 0 < x && x < a = s * x
    | otherwise = 0
