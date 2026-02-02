{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, foldl' )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [Int])
input = getInts >>= \[_, k] -> getInts >>= \hs -> return (k, hs)

output :: Int -> IO ()
output = print

solve :: (Int, [Int]) -> Int
solve (_, []) = 0
solve (k, h:hs) = finalDp
    where
        (finalDp, _) : _ = foldl' step [(0, h)] hs
        step history hi = take k ((newDp, hi) : history)
            where newDp = minimum [d + abs (hi - hj) | (d, hj) <- history]

main :: IO ()
main = input >>= output . solve
