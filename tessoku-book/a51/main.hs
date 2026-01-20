{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad ( replicateM )

data Stack a = Stack Int [a]

stackNew :: Stack a
stackNew = Stack 0 []

stackPush :: Stack a -> a -> Stack a
stackPush (Stack n xs) x = Stack (succ n) (x:xs)

stackPop :: Stack a -> Maybe (a, Stack a)
stackPop (Stack _ []) = Nothing
stackPop (Stack n (x:xs)) = Just (x, Stack (pred n) xs)

stackPeek :: Stack a -> Maybe a
stackPeek (Stack _ []) = Nothing
stackPeek (Stack _ (x:_)) = Just x

stackSize :: Stack a -> Int
stackSize (Stack n _) = n

input :: IO [String]
input = readLn >>= \q -> replicateM q getLine

output :: (Stack String, [String]) -> IO ()
output (_, xs) = putStrLn . unlines . reverse $ xs

solve :: [String] -> (Stack String, [String])
solve ws = foldl processQuery (stackNew, []) queries
    where
        queries = map parseQuery ws
        parseQuery :: String -> (String, String)
        parseQuery line = case words line of
            [i, s] -> (i, s)
            [i] -> (i, undefined)
            _ -> (undefined, undefined)
        processQuery :: (Stack String, [String]) -> (String, String) -> (Stack String, [String])
        processQuery (stack, xs) (i, s) = case i of
            "1" -> (stackPush stack s, xs)
            "2" -> case stackPeek stack of
                Just x -> (stack, x:xs)
                Nothing -> (stack, xs)
            "3" -> case stackPop stack of
                Just (_, stack') -> (stack', xs)
                Nothing -> (stack, xs)
            _ -> undefined

main :: IO ()
main = input >>= output . solve