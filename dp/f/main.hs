{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.Array ( Array, listArray, (!) )

input :: IO (String, String)
input = getLine >>= \s -> getLine >>= \t -> return (s, t)

output :: String -> IO ()
output = putStrLn

solve :: (String, String) -> String
solve (s, t) = reverse $ traceback slen tlen
    where
        slen = length s
        tlen = length t
        sarr = listArray (1, slen) s
        tarr = listArray (1, tlen) t

        rows = scanl step (replicate (tlen + 1) 0) s
        step prev c = scanl go 0 (zip3 t prev (drop 1 prev))
            where
                go left (d, diag, up)
                    | c == d = diag + 1
                    | otherwise = max left up

        dp :: Array (Int, Int) Int
        dp = listArray ((0, 0), (slen, tlen)) $ concat rows

        traceback 0 _ = ""
        traceback _ 0 = ""
        traceback i j
            | sarr ! i == tarr ! j = sarr ! i : traceback (i - 1) (j - 1)
            | dp ! (i - 1, j) >= dp ! (i, j - 1) = traceback (i - 1) j
            | otherwise = traceback i (j - 1)

main :: IO ()
main = input >>= output . solve
