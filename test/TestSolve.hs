module Main (main) where


import Mazelib.Generate
import Mazelib.Solve
import Data.List (elemIndices)
import System.Random (mkStdGen)
import Data.Either (fromRight)
import qualified System.Exit as Exit

isConnected :: Path -> Bool
isConnected path =
    let areConnected index = 
            let (r1, c1) = path !! index 
                (r2, c2) = path !! (index + 1)
            in abs (r2 - r1) + abs (c2 - c1) == 1
    in all areConnected [0..length path - 2]

noDuplicates :: Path -> Bool 
noDuplicates path = foldl
    (\ noDups cell -> noDups && null (drop 1 $ elemIndices cell path))
    True path

verifySolution :: Path -> Bool 
verifySolution path = isConnected path && noDuplicates path

main :: IO ()
main = do
    let generate gen = generateMaze AldousBroder (5, 5) (Just $ mkStdGen gen)
        mazes = map generate [1..5]
        solve (m, e) = cleanPath $ solveMaze RecursiveBacktracker m e Nothing
        extract = fromRight ([[]], ((0,0), (0,0)))
        extracted = map extract mazes
        solutions = map solve extracted
        result = all verifySolution solutions
    if result then Exit.exitSuccess else Exit.exitFailure
