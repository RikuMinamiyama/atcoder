{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad ( replicateM )
import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO [[Int]]
input = getInts >>= \[t] -> replicateM t $ getInts >>= \_ -> getInts >>= \rs -> return rs

output :: [Int] -> IO ()
output = mapM_ print

solve :: [[Int]] -> [Int]
solve = map go
    where
        go rs = sum $ zipWith (-) rs fs
            where
                fs = rightPass $ leftPass rs
                leftPass (r : rest) = scanl (\prev x -> min x (prev + 1)) r rest
                leftPass [] = []
                rightPass [] = []
                rightPass xs = snd $ foldr (\r (next, acc) -> let x = min r (next + 1) in (x, x : acc)) (maximum xs, []) xs

main :: IO ()
main = input >>= output . solve
