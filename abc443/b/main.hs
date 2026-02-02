{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, uncons )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int)
input = getInts >>= \[n, k] -> return (n, k)

output :: Int -> IO ()
output = print

solve :: (Int, Int) -> Int
solve (n, k) = case r of
    Just ((i, _), _) -> i
    Nothing -> undefined
    where
        r = uncons . dropWhile (\(_, n') -> n' < k) . scanl (\acc (i, n') -> (i, n' + snd acc)) (0, n) $ zip [1..] [succ n..]

main :: IO ()
main = input >>= output . solve
