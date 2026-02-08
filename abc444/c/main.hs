module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, sort )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO [Int]
input = getLine >> getInts >>= \as -> return as

output :: [Int] -> IO ()
output = putStrLn . unwords . map show

solve :: [Int] -> [Int]
solve as = sort $ case1 ++ case2
    where
        case1 = if even (length as) && all (== l) sums1 then [l] else []
            where
                l = case sums1 of
                    (s:_) -> s
                    _ -> undefined
                sums1 = zipWith (+) sortedAs (reverse sortedAs)
                sortedAs = sort as
        case2 = if even (length bs) && all (== m) sums2 then [m] else []
            where
                m = maximum as
                sums2 = zipWith (+) sortedBs (reverse sortedBs)
                bs = filter (/= m) as
                sortedBs = sort bs

main :: IO ()
main = input >>= output . solve
