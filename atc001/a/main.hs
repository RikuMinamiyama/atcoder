{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr )
import Control.Monad ( replicateM )
import Data.Array ( Array, listArray, assocs, (!) )
import Data.Graph ( Graph, buildG, reachable )

data Point = Start | Goal | Road | Fence deriving (Eq, Show)

type Maze = Array (Int, Int) Point

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int, [String])
input = getInts >>= \[h, w] -> replicateM h getLine >>= \mazeLines -> return (h, w, mazeLines)

output :: String -> IO ()
output = putStrLn

solve :: (Int, Int, [String]) -> String
solve (h, w, mazeLines) =
    if goalIdx `elem` reachable graph startIdx then "Yes" else "No"
    where
        maze = makeMaze h w mazeLines
        graph = buildGraph maze h w
        start = findPoint Start maze
        goal = findPoint Goal maze
        startIdx = fst start * w + snd start
        goalIdx = fst goal * w + snd goal

c2p :: Char -> Point
c2p '#' = Fence
c2p '.' = Road
c2p 's' = Start
c2p 'g' = Goal
c2p _ = undefined

makeMaze :: Int -> Int -> [String] -> Maze
makeMaze h w inputLines = listArray ((0, 0), (h - 1, w - 1)) [c2p c | c <- concat inputLines]

buildGraph :: Maze -> Int -> Int -> Graph
buildGraph maze h w = buildG (0, maxIdx) edges
    where
        maxIdx = h * w - 1
        edges = [(i * w + j, ni * w + nj) |
                 i <- [0..h-1], j <- [0..w-1],
                 maze ! (i, j) /= Fence,
                 (di, dj) <- [(0, 1), (0, -1), (1, 0), (-1, 0)],
                 let ni = i + di,
                 let nj = j + dj,
                 ni >= 0, ni < h, nj >= 0, nj < w,
                 maze ! (ni, nj) /= Fence]

findPoint :: Point -> Maze -> (Int, Int)
findPoint p maze = case [pos | (pos, point) <- assocs maze, point == p] of
    [] -> undefined
    x:_ -> x

main :: IO ()
main = input >>= output . solve