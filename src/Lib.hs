module Lib
    ( demoGame
    ) where
import Stones.Laws
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Stones.UI.CLI

grid = gridFromLists [
            [1, 1, 1, 1],
            [0, 0, 0, 0],
            [2, 2, 2, 2]
            ]

demoGame :: IO ()
demoGame = do
    clearCLI
    putStrLn "Welcome to a Haskell Game of Stones, press enter.."
    iterateGame grid

iterateGame :: StoneGrid -> IO()
iterateGame grid = do 
            getLine
            clearCLI
            putStrLn "Press enter to continue.."
            printGrid grid 
            iterateGame grid'
    where
        move = Move (XY 0 0) (XY 0 1)
        grid' = makeMove grid move
