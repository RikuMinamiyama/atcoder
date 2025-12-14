main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- getLine
    putStrLn $ replicate (n - length s) 'o' ++ s