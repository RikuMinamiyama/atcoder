{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )
import Data.Graph ( Graph, buildG, reachable )
import Data.Set ( fromList, toList )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [[Int]])
input = getInts >>= \[n, m] -> replicateM m getInts >>= \abs' -> return (n, abs')

output :: String -> IO ()
output = putStrLn

solve :: (Int, [[Int]]) -> String
solve (n, abs') = if length (reachable (buildGraph n abs') 1) == n
    then "The graph is connected."
    else "The graph is not connected."

buildGraph :: Int -> [[Int]] -> Graph
buildGraph n abs' = buildG bounds edges
    where
        bounds = (1, n)
        edges = toList . fromList $ concatMap (\[a, b] -> [(a, b), (b, a)]) abs'

main :: IO ()
main = input >>= output . solve