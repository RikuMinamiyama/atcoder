module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
    [n, m] <- getInts
    mapM_ putStrLn $ replicate (min n m) "OK" ++ replicate (max 0 (n - m)) "Too Many Requests"

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine