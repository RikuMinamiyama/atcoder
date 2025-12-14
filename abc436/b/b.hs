import Data.Array

main :: IO ()
main = do
    n <- readLn :: IO Int
    let result = operation n (0, (n - 1) `div` 2) 1 (array ((0, 0), (n-1, n-1)) [((i, j), 0) | i <- [0..n-1], j <- [0..n-1]])
    mapM_ (putStrLn . unwords . map show) [[result ! (i, j) | j <- [0..n-1]] | i <- [0..n-1]]

operation :: Int -> (Int, Int) -> Int -> Array (Int, Int) Int -> Array (Int, Int) Int
operation n (r, c) k arr
    | n * n == k + 1 = arr
    | k == 1 = operation n (r, c) (k + 1) (arr // [((r, c), k)])
    | otherwise = operation n (r', c') (k + 1) (arr // [((r', c'), k)])
        where (r', c') = if arr ! ((r-1) `mod` n, (c+1) `mod` n) == 0 then ((r-1) `mod` n, (c+1) `mod` n) else ((r+1) `mod` n, c)