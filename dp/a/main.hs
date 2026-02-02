module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, foldl' )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO [Int]
input = getLine >> getInts >>= \hs -> return hs

output :: Int -> IO ()
output = print

solve :: [Int] -> Int
solve (h1:h2:rest) = dp
    where
        (dp, _, _, _) = foldl' step (abs (h2 - h1), 0, h2, h1) rest
        step (dp1, dp2, hi1, hi2) h = (min (dp1 + abs (h - hi1)) (dp2 + abs (h - hi2)), dp1, h, hi1)
solve _ = undefined

main :: IO ()
main = input >>= output . solve
