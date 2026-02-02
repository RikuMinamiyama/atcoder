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
solve (k, h:hs) = dp
    where
        (dp, _) : _ = foldl' step [(0, h)] hs
        step history hi = take k ((dp', hi) : history)
            where
                dp' = minimum [d + abs (hi - hj) | (d, hj) <- history]
solve _ = undefined

main :: IO ()
main = input >>= output . solve
