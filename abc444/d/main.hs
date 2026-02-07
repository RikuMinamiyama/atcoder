{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, intercalate )
import Data.Array ( accumArray, elems )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO [Int]
input = getLine >> getInts >>= return

output :: String -> IO ()
output = putStrLn

solve :: [Int] -> String
solve as = intercalate ""
         . map show
         . dropWhile (== 0)
         . reverse
         . kuriagari 0
         . init
         . scanr (+) 0
         . elems
         . accumArray (+) 0 (1, maximum as)
         $ [(a, 1) | a <- as]

kuriagari :: Int -> [Int] -> [Int] 
kuriagari acc [] = [acc]
kuriagari acc (x:xs) = (acc + x) `mod` 10 : kuriagari ((acc + x) `div` 10) xs

main :: IO ()
main = input >>= output . solve
