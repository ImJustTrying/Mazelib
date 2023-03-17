{-|
Module      : Mazelib.Generate
Description : A library for maze generation and solving
Copyright   : (c) Kevin Vicente, 2023
License     : GPL-3
Maintainer  : kvicente@fastmail.com
Stability   : experimental
Portability : portable
-}

module Mazelib.Generate
    (
      GenerationMethod(..)
    , Maze
    , mazeToString
    , generateMaze
    , generateExits
    ) where


import qualified System.Random as Rand
import Data.Maybe (fromMaybe)


-- True = wall, False = empty, stored in row-major
type Maze = [[Bool]]
type Exits = ((Int, Int), (Int, Int))
data GenerationMethod =
    AldousBroder |
    Backtracking


mazeToString :: Maze -> Exits -> String
mazeToString [] _ = ""
mazeToString maze (start, end) = 
    let ncols = length (head maze) + 1
        wallChar = '+'
        rowToStr row = map (\x -> if x then wallChar else ' ') row ++ "\n"
        -- init to remove the final newline
        mazeStr = init $ concatMap rowToStr maze
        startStrIndex = fst start * ncols + snd start
        endStrIndex = fst end * ncols + snd end
        (beforeStart, _:afterStart) = splitAt startStrIndex mazeStr
        newMazeStr = beforeStart ++ "S" ++ afterStart
        (beforeEnd, _:afterEnd) = splitAt endStrIndex newMazeStr
    in beforeEnd ++ "E" ++ afterEnd

-- UNSAFE 
-- Since this is an internal function, we use (!!) and don't check the range
randOddRange :: (Int, Int) -> Rand.StdGen -> (Int, Rand.StdGen)
randOddRange (low, high) gen =
    let odds = filter odd [low..high]
        (i, gen') = Rand.uniformR (0, length odds - 1) gen
    in (odds !! i, gen')

-- UNSAFE
replace :: (Int, Int) -> [[t]] -> t -> [[t]]
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

getNeighbors :: (Int, Int) -> Maze -> Bool -> Int -> [(Int, Int)]
getNeighbors (row, col) maze iswall distance =
    let (nrows, ncols) = (length maze, length $ head maze)
        neighbors =
            [(row - distance, col), (row + distance, col),
             (row, col - distance), (row, col + distance)]
        inBounds (r, c) =
            r > 0 && r < nrows - 1 &&
            c > 0 && c < ncols - 1 &&
            maze !! r !! c == iswall
    in filter inBounds neighbors

generateExits :: Maze -> Maybe Rand.StdGen -> (Maybe Exits, Rand.StdGen)
generateExits maze gen = 
    let (nrows, ncols) = (length maze, length $ head maze)
        potentials = 
            [(0,         i) | i <- [1..ncols-2]]
         ++ [(nrows - 1, i) | i <- [1..ncols-2]]
         ++ [(i,         0) | i <- [1..nrows-2]]
         ++ [(i, ncols - 1) | i <- [1..nrows-2]]
        -- Filter based on whether each cell's adjacent cells are walls
        filtered = filter (\ n -> null $ getNeighbors n maze True 1) potentials
        (mstart, gen') = chooseRand (fromMaybe (Rand.mkStdGen 0) gen) filtered
        start = fromMaybe (1,1) mstart
        (mend, gen'') = chooseRand gen' [x | x <- filtered, x /= start]
        end = fromMaybe (nrows - 1, ncols - 1) mend
    in if nrows < 3 || ncols < 3 then
        (Nothing, gen'')
    else 
        (Just (start, end), gen'')

genAldousBroder :: Rand.StdGen -> Int -> (Int, Int) -> Maze -> (Maze, Rand.StdGen)
genAldousBroder gen numVisited (row, col) maze = 
    let numCells = 
            ((length maze - 1) `div` 2) * (length (head maze) - 1) `div` 2
        newMaze = replace (row, col) maze False
        (v, gen') = chooseRand gen $ getNeighbors (row, col) maze True 2
    in if numVisited >= numCells then 
        (newMaze, gen')
    else case v of
        Just (row', col') ->
            let midrow = (row + row') `div` 2
                midcol = (col + col') `div` 2
            in genAldousBroder gen' (numVisited + 1) (row', col') $
                replace (midrow, midcol) newMaze False
        Nothing ->
            let emptyNeighbors = getNeighbors (row, col) maze False 2
                (emptyNeighbor, newGen) = chooseRand gen' emptyNeighbors
            in case emptyNeighbor of
                Just n ->
                    genAldousBroder newGen numVisited n newMaze
                Nothing -> (newMaze, newGen)


generateMaze :: GenerationMethod -> (Int, Int) -> Maybe Rand.StdGen -> Either String (Maze, Exits)
generateMaze method (nrows, ncols) generator = 
    if nrows < 3 || ncols < 3 then
        Left "Error: mazes must be at least 3x3"
    else let 
        n = 2 * ncols + 1 
        m = 2 * nrows + 1
        (col, g) = randOddRange (1, n - 1) gen
        (row, g') = randOddRange (1, m - 1) g 
    in case method of
        AldousBroder -> let 
            (maze, g1) = genAldousBroder g' 1 (row, col) walls
            (exits, _) = generateExits maze $ Just g1
            in case exits of 
                Nothing -> Left "Error: invalid maze generated"
                Just e -> Right (maze, e)
        _            -> Right (walls, ((0,0),(0,0)))
    where
        gen = fromMaybe (Rand.mkStdGen 1) generator
        walls = [[True | _ <- [1..2*nrows+1]] | _ <- [1..2*ncols+1]]
