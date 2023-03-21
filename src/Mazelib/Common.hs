module Mazelib.Common where

import qualified System.Random as Rand
import Data.List (nub)

-- import qualified Data.Array as Arr
-- type Maze = Arr.Array (Int, Int) Bool

-- | True = wall, False = empty, stored in row-major
type Maze = [[Bool]]
-- | (start, end) where each is stored as (row, column)
type Exits = ((Int, Int), (Int, Int))
-- | A list of coordinates stored as (row, column)
type Path = [(Int, Int)]

-- | A convenience method that will pretty print a given maze, along with it's
-- entrance, exit, and possibly a path through the maze
mazeToString :: Maze -> Exits -> Maybe Path -> String
mazeToString [] _ _ = ""
mazeToString maze (start, end) solution = 
    let ncols = length (head maze) + 1
        rowToStr row = map (\ x -> if x then '+' else ' ') row ++ "\n"
        -- init to remove the final newline
        mazeStr = init $ concatMap rowToStr maze
        startStrIndex = fst start * ncols + snd start
        endStrIndex = fst end * ncols + snd end
        replaceInStr index str char =
            let (before, _:after) = splitAt index str
            in before ++ char ++ after
        startStr = replaceInStr startStrIndex mazeStr "S"
        endStr = replaceInStr endStrIndex startStr "E"
    in case solution of 
        Just p -> let
            indices =
                map (\ (i,j) -> i * ncols + j) $
                filter (\ i -> not (i == start || i == end)) (nub p)
            in foldl (\ str i -> replaceInStr i str "O") endStr indices
        Nothing -> endStr

chooseRand :: Rand.StdGen -> [t] -> (Maybe t, Rand.StdGen)
chooseRand gen [] = (Nothing, gen)
chooseRand gen l =
    let (i, gen') = Rand.uniformR (0, length l - 1) gen
    in (Just (l !! i), gen')

getNeighbors :: (Int, Int) -> Maze -> Bool -> Bool -> Int -> [(Int, Int)]
getNeighbors (row, col) maze isWall avoidBorder distance =
    let (nrows, ncols) = (length maze, length $ head maze)
        neighbors =
            [(row - distance, col), (row + distance, col),
             (row, col - distance), (row, col + distance)]
        lt = if avoidBorder then (<) else (<=)
        inBoundsAndIsWall (r, c) =
            0 `lt` r && r `lt` (nrows - 1) &&
            0 `lt` c && c `lt` (ncols - 1) &&
            maze !! r !! c == isWall
    in filter inBoundsAndIsWall neighbors

-- UNSAFE
-- Since this is an internal function, we use (!!) and don't check the range
replace :: (Int, Int) -> [[t]] -> t -> [[t]]
replace (row, col) maze b = 
    let rowBefore = take col (maze !! row)
        rowAfter = drop (col + 1) (maze !! row)
        rowsBefore = take row maze
        rowsAfter = drop (row + 1) maze
    in rowsBefore ++ [rowBefore ++ [b] ++ rowAfter] ++ rowsAfter
