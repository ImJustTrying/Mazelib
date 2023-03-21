module Main (main) where


import System.Exit as Exit
import System.Random (mkStdGen)
import Mazelib.Generate


oddsEmpty :: Maze -> Bool
oddsEmpty maze =
    let (nrows, ncols) = (length maze, length $ head maze)
        oddRows = filter odd [1..nrows-1]
        oddCols = filter odd [1..ncols-1]
        positions = zip oddRows oddCols
    in foldl 
        (\ isEmpty (row, col) -> isEmpty && not ((maze !! row) !! col))
        True positions

evensWalled :: Maze -> Bool
evensWalled maze =
    let (nrows, ncols) = (length maze, length $ head maze)
        evenRows = filter even [2..nrows-1]
        evenCols = filter even [2..ncols-1]
        positions = zip evenRows evenCols
    in foldl 
        (\ isWall (row, col) -> isWall && (maze !! row) !! col)
        True positions

borderWalled :: Maze -> Bool 
borderWalled maze =
    let (nrows, ncols) = (length maze, length $ head maze)
        positions = 
            [(0,         i) | i <- [0..ncols-1]]
         ++ [(nrows - 1, i) | i <- [0..ncols-1]]
         ++ [(i,         0) | i <- [0..nrows-1]]
         ++ [(i, ncols - 1) | i <- [0..nrows-1]]
    in foldl 
        (\ isWall (row, col) -> isWall && (maze !! row) !! col)
        True positions

verifyMaze :: Maze -> Bool
verifyMaze maze = oddsEmpty maze && evensWalled maze && borderWalled maze

main :: IO ()
main = do 
    let generator gen = generateMaze AldousBroder (5,5) $ Just (mkStdGen gen)
        results = map generator [1..5]
        verify result = case result of 
            Left _ -> False
            Right (maze, _) -> verifyMaze maze
        endResult = all verify results
    if endResult then 
        Exit.exitSuccess
    else 
        Exit.exitFailure
