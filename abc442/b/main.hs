module Main where

import Control.Monad ( replicateM )

input :: IO [Int]
input = readLn >>= \q -> replicateM q readLn >>= \qs -> return qs

output :: (Int, State, [String]) -> IO ()
output (_, _, xs) = putStrLn . unlines . reverse $ xs

data State = Paused | Playing deriving (Eq, Show)

solve :: [Int] -> (Int, State, [String])
solve = foldl go (0, Paused, [])
    where
        go (n, state, xs) q
            | q == 1 = (succ n, state, if isPlaying (succ n) state then "Yes" : xs else "No" : xs)
            | q == 2 = case n of
                0 -> (0, state, "No" : xs)
                _ -> (pred n, state, if isPlaying (pred n) state then "Yes" : xs else "No" : xs)
            | q == 3 = (n, switchState state, if isPlaying n (switchState state) then "Yes" : xs else "No" : xs)
            | otherwise = undefined
            where
                isPlaying m s = m >= 3 && s == Playing
                switchState Paused = Playing
                switchState Playing = Paused


main :: IO ()
main = input >>= output . solve
