{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Data.Map.Strict ( Map, fromListWith, findWithDefault, toList )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [Int])
input = readLn >>= \n -> getInts >>= \as -> return (n, as)

output :: String -> IO ()
output = putStrLn

solve :: (Int, [Int]) -> String
solve (n, as) = undefined

main :: IO ()
main = input >>= output . solve
