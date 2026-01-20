{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (String, String, [String])
input = getLine >> getLine >>= \s -> getLine >>= \t -> readLn >>= \q -> replicateM q getLine >>= \ws -> return (s, t, ws)

output :: [String] -> IO ()
output = putStrLn . unlines

solve :: (String, String, [String]) -> [String]
solve (s, t, ws) = map (go s t) ws
    where
        go s' t' w
            | isTakahashi && isAoki = "Unknown"
            | isTakahashi = "Takahashi"
            | isAoki = "Aoki"
            | otherwise = undefined
            where
                isTakahashi = all (`elem` s') w
                isAoki = all (`elem` t') w

main :: IO ()
main = input >>= output . solve