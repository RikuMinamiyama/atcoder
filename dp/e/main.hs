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
solve (w, wvs) = fst . last $ ws
    where
        ws = foldl' step [(0,0)] . map toPair $ wvs
            where
                step vws' (a, b) = zip [0..sumV]
                                 . elems
                                 . accumArray min 1000000001 (0, sumV)
                                 $ vws''
                    where
                        vws'' = concatMap (\(v', w') -> (v', w') : [(v' + b, w' + a) | w' + a <= w]) vws'
                        sumV = maximum . map fst $ vws''

toPair :: [Int] -> (Int, Int)
toPair (a:b:[]) = (a, b)
toPair _ = undefined

main :: IO ()
main = input >>= output . solve
