{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, foldl' )
import Control.Monad ( replicateM )
import Data.Array ( elems, accumArray )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, [[Int]])
input = getInts >>= \[n, w] -> replicateM n getInts >>= \wvs -> return (w, wvs)

output :: Int -> IO ()
output = print

solve :: (Int, [[Int]]) -> Int
solve (w, wvs) = maximum . map snd $ vs
    where
        vs = foldl' step [(0,0)] (map toPair wvs)
            where
                step wvs' (a, b) = zip [0..w]
                                 . elems
                                 . accumArray max 0 (0, w)
                                 . concat
                                 $ [(w', v') : if w' + a <= w then [(w' + a, v' + b)] else [] | (w', v') <- wvs']

toPair :: [Int] -> (Int, Int)
toPair (a:b:[]) = (a, b)
toPair _ = undefined

main :: IO ()
main = input >>= output . solve
