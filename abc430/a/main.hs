module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.List (unfoldr)

main :: IO ()
main = do
    [a, b, c, d] <- getInts
    putStrYesNo $ (a > c) || (b <= d)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

putStrYesNo :: Bool -> IO ()
putStrYesNo True = putStrLn "No"
putStrYesNo False = putStrLn "Yes"