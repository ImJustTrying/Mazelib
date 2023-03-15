module Mazelib 
    (
      GenerationMethod(..)
    , Maze
    , toString
    , generateMaze
    ) where

import qualified System.Random as Rand
import Data.Maybe (fromMaybe)

-- True = wall, False = empty, stored in row-major
type Maze = [[Bool]]
data GenerationMethod =
    AldousBroder |
    Backtracking

toString :: Maze -> String
toString [] = ""
toString maze = 
    let wallChar = '+'
        rowToStr row = map (\x -> if x then wallChar else ' ') row ++ "\n"
    -- init to remove the final newline
    in init $ concatMap rowToStr maze

-- Generate random odd numbers in a given range, returning a list and generator
randOddsRange :: Int -> (Int, Int) -> Rand.StdGen -> ([Int], Rand.StdGen)
randOddsRange 0 (_, _) g = ([], g)
randOddsRange n (low, high) generator =
    let odds = filter odd [low..high]
        r = randOddsRange (n - 1) (low, high) generator
        -- Use new random generator -- `snd r` -- for next flip
        p = Rand.uniformR (0, length odds - 1) (snd r)
    in (fst r ++ [odds !! fst p], snd p)

randOddRange :: (Int, Int) -> Rand.StdGen -> (Int, Rand.StdGen)
randOddRange (low, high) gen =
    let odds = filter odd [low..high]
        (i, gen') = Rand.uniformR (0, length odds) gen
    in (odds !! i, gen')

replace :: (Int, Int) -> Maze -> Bool -> Maze 
replace (row, col) maze b = 
    let rowBefore = take col (maze !! row)
        rowAfter = drop (col + 1) (maze !! row)
        rowsBefore = take row maze
        rowsAfter = drop (row + 1) maze
    in rowsBefore ++ [rowBefore ++ [b] ++ rowAfter] ++ rowsAfter

chooseRand :: Rand.StdGen -> [t] -> (Maybe t, Rand.StdGen)
chooseRand gen [] = (Nothing, gen)
chooseRand gen l =
    let (i, gen') = Rand.uniformR (0, length l - 1) gen
    in (Just (l !! i), gen')

getNeighbors :: (Int, Int) -> Maze -> Bool -> [(Int, Int)]
getNeighbors (row, col) m iswall =
    let (nrows, ncols) = (length m, length $ head m)
        neighbors =
            [(row - 2, col), (row + 2, col), (row, col - 2), (row, col + 2)]
        inBounds (r, c) =
            r > 0 && r < nrows - 1 &&
            c > 0 && c < ncols - 1 &&
            m !! r !! c == iswall
    in filter inBounds neighbors

genAldousBroder :: Rand.StdGen -> Int -> (Int, Int) -> Maze -> Maze
genAldousBroder gen numVisited (row, col) maze = 
    let newMaze = replace (row, col) maze False
        neighbors = getNeighbors (row, col) maze True
        (v, gen') = chooseRand gen neighbors
    in case v of
        Just (row', col') ->
            let midrow = (row + row') `div` 2
                midcol = (col + col')`div` 2
            in genAldousBroder gen' (numVisited + 1) (row', col') $
                replace (midrow, midcol) newMaze False
        Nothing -> newMaze


generateMaze :: GenerationMethod -> (Int, Int) -> Maybe Rand.StdGen -> Either String Maze
generateMaze method (nrows, ncols) generator = 
    if nrows < 3 || ncols < 3 then
        Left "Error: mazes must be at least 3x3"
    else case method of
        AldousBroder -> Right $
            let n = 2 * ncols + 1 
                m = 2 * nrows + 1
                (col, g) = randOddRange (1, n - 1) gen
                (row, g') = randOddRange (1, m - 1) g 
            in genAldousBroder g' 0 (row, col) walls
        _            -> Right walls
    where
        gen = fromMaybe (Rand.mkStdGen 123) generator
        walls = [[True | _ <- [1..2*nrows+1]] | _ <- [1..2*ncols+1]]
