{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO ()
input = return ()

output :: String -> IO ()
output = putStrLn

solve :: () -> String
solve () = ""

main :: IO ()
main = input >>= output . solve
