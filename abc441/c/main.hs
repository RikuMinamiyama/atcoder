{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, sort )
import Data.Array ( Array, listArray, (!), bounds )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int, Int, [Int])
input = getInts >>= \[n, k, x] -> getInts >>= \as -> return (n, k, x, as)

output :: Int -> IO ()
output = print

solve :: (Int, Int, Int, [Int]) -> Int
solve (n, k, x, as) = 
    case lowerBoundIdx arr (>= x) of
        Just idx -> idx + (n - k) + 1  -- 1-indexed
        Nothing -> -1
    where
        arr = listArray (0, k - 1) . scanl1 (+) . drop (n - k) . reverse . sort $ as


-- 二分探索で条件を満たす最小のインデックスを探す
lowerBoundIdx :: Array Int Int -> (Int -> Bool) -> Maybe Int
lowerBoundIdx arr p = go lo hi
    where
        (lo, hi) = bounds arr
        go l h
            | l > h = Nothing
            | otherwise =
                let mid = (l + h) `div` 2
                    val = arr ! mid
                in if p val
                    then case go l (mid - 1) of
                        Nothing -> Just mid
                        Just idx -> Just idx
                    else go (mid + 1) h

main :: IO ()
main = input >>= output . solve