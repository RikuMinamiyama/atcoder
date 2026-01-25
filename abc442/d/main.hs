{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Data.Char ( isSpace )
import Data.List ( unfoldr, foldl' )
import Control.Monad ( replicateM )

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

input :: IO (Int, Int, [Int], [String])
input = getInts >>= \[n, q] -> getInts >>= \as -> replicateM q getLine >>= \queries -> return (n, q, as, queries)

output :: ([Int], [Int]) -> IO ()
output = putStrLn . unlines . map show . snd

solve :: (Int, Int, [Int], [String]) -> ([Int], [Int])
solve (n, q, as, queries) = foldl go (as, []) queries
    where
        go (list, results) query = case query of
            "1":" ":x:_ -> (update (read x) (fromListToSegtree list), results)
            "2":" ":l:" ":r:_ -> (list, query (read l) (read r) (fromListToSegtree list) : results)
            _ -> undefined

data Segtree a = 
  Leaf {
    index :: Int,
    val :: a
  } | 
  Node {
    lTree :: Segtree a,
    rTree :: Segtree a,
    lBound :: Int,
    rBound :: Int,
    val :: a
  }

buildTree :: Monoid a => Int -> Int -> Segtree a
buildTree l r
  | l + 1 == r = Leaf l mempty
  | otherwise = Node left right l r (val left <> val right)
  where
    m = (l + r) `div` 2
    left = buildTree l m
    right = buildTree m r

fromListToSegtree :: Monoid a => [a] -> Segtree a
fromListToSegtree as = foldl' (\cur (i, a) -> update i a cur) (buildTree 1 (length as + 1)) (zip [1..] as)

-- x と x+1 の要素を入れ替える
update :: Monoid a => Int -> Segtree a -> Segtree a
update x (Leaf j _) 
  | j == x = Leaf j mempty
  | otherwise = unreachable
update x (Node lt rt lb rb _)
  | x < m = let lt' = update x lt in Node lt' rt lb rb (val lt' <> val rt)
  | otherwise = let rt' = update x rt in Node lt rt' lb rb (val lt <> val rt')
  where 
    m = (lb + rb) `div` 2

query :: Monoid a => Int -> Int -> Segtree a -> a
query ql qr (Leaf i v)
  | ql <= i && i < qr = v
  | otherwise = mempty
query ql qr (Node lt rt lb rb v)
  | qr <= lb || rb <= ql = mempty
  | ql <= lb && rb <= qr = v
  | otherwise = query ql qr lt <> query ql qr rt

unreachable :: a
unreachable = error "unreachable"

main :: IO ()
main = input >>= output . solve
