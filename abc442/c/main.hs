{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )
import Data.Array ( accumArray, elems )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [(Int, Int)])
input = getInts >>= \[n, m] -> replicateM m (getInts >>= \[a, b] -> return (a, b)) >>= \ab -> return (n, ab)

output :: [Int] -> IO ()
output = putStrLn . unwords . map show

solve :: (Int, [(Int, Int)]) -> [Int]
solve (n, ab) = map (flip nCr 3 . (n - 1 -)) (researchers n ab)

researchers :: Int -> [(Int, Int)] -> [Int]
researchers n = elems
              . accumArray (+) 0 (1, n)
              . concatMap (\(a, b) -> [(a, 1), (b, 1)])

nCr :: Integral a => a -> a -> a
nCr n r
    | n < 0 || r < 0 = undefined
    | n < r = 0
    | n == 0 || r == 0 || n == r = 1
    | otherwise = iter 1 n 1
    where
        r' = min r (n-r)
        iter p m q
            | q > r' = p
            | otherwise = iter (p * m `div` q) (pred m) (succ q)

main :: IO ()
main = input >>= output . solve
