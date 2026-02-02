{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [Int])
input = getInts >>= \[_, x] -> getInts >>= \as -> return (x, as)

output :: String -> IO ()
output = putStrLn

solve :: (Int, [Int]) -> String
solve (x, as) = if x `elem` as then "Yes" else "No"

main :: IO ()
main = input >>= output . solve
