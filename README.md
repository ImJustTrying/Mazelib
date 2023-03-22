# Mazelib
A library for generating and solving 2D mazes in Haskell

```
let result = generateMaze AldousBroder (3, 3) $ Just (mkStdGen 123)
in case result of
    Left e ->
        do putStrLn e
    Right (maze, exits) ->
        let path = cleanPath $ solveMaze maze exits Nothing
        in do putStrLn $ mazeToString maze exits (Just path)
Output:
+++++++
SOOOOO+
+++++O+
+OOO+O+
+O+O+O+
+O+OOO+
+E+++++
```

An (N x M) Maze is represented in memory as a (2N + 1 x 2M + 1) boolean matrix. 
In an idealized maze, walls are 0 width, but in memory they are width 1, thus 
the larger size. From this we infer that coordinates (x, y) where both x and y 
are odd integers map to the coordinates in the ideal maze -- i.e. 
`(x, y) -> ((x - 1) / 2, (y - 1) / 2)`.


