{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, foldl' )
-- import Data.Array ( listArray, (!) )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [Int])
input = readLn >>= \n -> getInts >>= \hs -> return (n, hs)

output :: Int -> IO ()
output = print

solve :: (Int, [Int]) -> Int
solve (1, _) = 0
solve (_, h1:h2:rest) = result
    where
        initial = (abs (h2 - h1), 0, h2, h1)
        step (dp1, dp2, hi1, hi2) h = (min (dp1 + abs (h - hi1)) (dp2 + abs (h - hi2)), dp1, h, hi1)
        (result, _, _, _) = foldl' step initial rest

-- solve :: (Int, [Int]) -> Int
-- solve (n, hs) = dp ! n
--     where
--         arr = listArray (1, n) hs

--         dp = listArray (1, n) [cost i | i <- [1..n]]

--         cost 1 = 0
--         cost 2 = abs (arr ! 2 - arr ! 1)
--         cost i = min (dp ! s + abs (arr ! i - arr ! s)) (dp ! t + abs (arr ! i - arr ! t))
--             where
--                 s = pred i
--                 t = (pred . pred) i

main :: IO ()
main = input >>= output . solve
