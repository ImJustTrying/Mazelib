module Main where

import Mazelib

main :: IO ()
main = do
    let gen = generateMaze Backtracking (3, 3) Nothing
    putStrLn $ case gen of
        Left e -> e 
        Right m -> toString m
