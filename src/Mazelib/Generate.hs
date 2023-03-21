{-|
Module      : Mazelib.Generate
Description : A library for maze generation
Copyright   : (c) Kevin Vicente, 2023
License     : GPL-3
Maintainer  : kvicente@fastmail.com
Stability   : experimental
Portability : portable
-}

module Mazelib.Generate
    (
      GenerationMethod(..)
    , Path
    , Maze
    , Exits 
    , mazeToString
    , generateMaze
    , generateExits
    ) where


import qualified System.Random as Rand
import Data.Maybe (fromMaybe)
import Mazelib.Common


-- | Various methods for generating a 2D maze
data GenerationMethod =
    -- | The Aldous Broder method works as follows:
    -- Preconditions: maze is filled with walls, set current cell to a random 
    -- cell
    --
    --    (1) Set the current cell to be empty (no wall)
    --    (2) Pick a random neighboring wall cell 
    --    (3) If the cell exists, then set that cell to be the current cell and go 
    --    back to step 1
    --    (4) Otherwise, choose a neighboring empty cell, set it to the current,
    --    and go back to step 1
    --
    AldousBroder

-- | This will generate new exits for a given maze if possible
generateExits ::
    Maze              ->       -- ^ The maze
    Maybe Rand.StdGen ->       -- ^ Possibly an RNG, or the default
    (Maybe Exits, Rand.StdGen) -- ^ Possibly the new exits and a new RNG
generateExits maze gen = 
    let (nrows, ncols) = (length maze, length $ head maze)
        potentials = 
            [(0,         i) | i <- [1..ncols-2]]
         ++ [(nrows - 1, i) | i <- [1..ncols-2]]
         ++ [(i,         0) | i <- [1..nrows-2]]
         ++ [(i, ncols - 1) | i <- [1..nrows-2]]
        -- Filter based on whether each cell's adjacent cells are walls
        neighbors n = getNeighbors n maze True True 1
        filtered = filter (null . neighbors) potentials
        (mstart, gen') = chooseRand (fromMaybe (Rand.mkStdGen 0) gen) filtered
        start = fromMaybe (1,1) mstart
        (mend, gen'') = chooseRand gen' [x | x <- filtered, x /= start]
        end = fromMaybe (nrows - 1, ncols - 1) mend
    in if nrows < 3 || ncols < 3 then
        (Nothing, gen'')
    else 
        (Just (start, end), gen'')

-- UNSAFE 
-- Since this is an internal function, we use (!!) and don't check the range
randOddRange :: (Int, Int) -> Rand.StdGen -> (Int, Rand.StdGen)
randOddRange (low, high) gen =
    let odds = filter odd [low..high]
        (i, gen') = Rand.uniformR (0, length odds - 1) gen
    in (odds !! i, gen')

genAldousBroder ::
    Rand.StdGen ->
    Int         ->
    (Int, Int)  ->
    Maze        ->
    (Maze, Rand.StdGen)
genAldousBroder gen numVisited (row, col) maze = 
    let numCells = 
            ((length maze - 1) `div` 2) * (length (head maze) - 1) `div` 2
        newMaze = replace (row, col) maze False
        (v, gen') = chooseRand gen $ getNeighbors (row, col) maze True True 2
    in if numVisited >= numCells then 
        (newMaze, gen')
    else case v of
        Just (row', col') ->
            let midrow = (row + row') `div` 2
                midcol = (col + col') `div` 2
            in genAldousBroder gen' (numVisited + 1) (row', col') $
                replace (midrow, midcol) newMaze False
        Nothing ->
            let emptyNeighbors = getNeighbors (row, col) maze False True 2
                (emptyNeighbor, newGen) = chooseRand gen' emptyNeighbors
            in case emptyNeighbor of
                Just n ->
                    genAldousBroder newGen numVisited n newMaze
                Nothing -> (newMaze, newGen)


-- | The function used to randomly generate a maze, and it's entrance and exit
generateMaze ::
    GenerationMethod  ->        -- ^ The desired generation method
    (Int, Int)        ->        -- ^ The size of the maze as (rows, columns)
    Maybe Rand.StdGen ->        -- ^ Possibly an RNG, or use the default
    Either String (Maze, Exits)
    -- ^ Either an error message (Left) or the generated maze and exits (Right)
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
        -- _            -> Right (walls, ((0,0),(0,0)))
    where
        gen = fromMaybe (Rand.mkStdGen 1) generator
        walls = [[True | _ <- [1..2*nrows+1]] | _ <- [1..2*ncols+1]]
