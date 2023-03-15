module Mazelib (
    GenerationMethod(..),
    toString,
    generateMaze
) where

import System.Random
-- import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

type Maze = [[Bool]]
data GenerationMethod =
    AldousBroder |
    Backtracking

toString :: Maze -> String
toString [] = ""
toString (row:rows) = 
    let wallChar = '+'
        rowToStr = map (\x -> if x then wallChar else ' ')
    in rowToStr row ++ "\n" ++ toString rows

coinFlips :: Uniform t => Int -> StdGen -> ([t], StdGen)
coinFlips 0 g = ([], g)
coinFlips n generator = 
    let r = coinFlips (n - 1) generator
        -- Use new random generator for next flip
        p = uniform (snd r)
    in (fst r ++ [fst p], snd p)

generateMaze :: GenerationMethod -> (Int, Int) -> Maybe StdGen -> Either String Maze
generateMaze method (n, m) generator
    | n < 3 || m < 3 =
        Left "Error: mazes must be at least 3x3"
    | n >= 3 && m >= 3 =
        case method of
            AldousBroder -> Right $ genAldousBroder (n, m) gen
            _            -> Right [[True | _ <- [1..n]] | _ <- [1..m]]
        where gen = fromMaybe (mkStdGen 123) generator

genAldousBroder :: (Int, Int) -> StdGen -> Maze
genAldousBroder (n, m) gen = [[True | _ <- [1..n]] | _ <- [1..m]]
