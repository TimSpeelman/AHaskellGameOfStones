module Lib
    ( demoGame
    ) where
import Stones.Laws
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Stones.UI.CLI
import Stones.Data.Game
import Stones.Strategy.Aggressive

grid = gridFromLists [
            [1, 1, 1, 1],
            [0, 0, 0, 0],
            [2, 2, 2, 2]
            ]
game = Game (Player 1) [(Player 1), (Player 2)] grid

demoGame :: IO ()
demoGame = do
    clearCLI
    putStrLn "Welcome to a Haskell Game of Stones, press enter.."
    iterateGame game

iterateGame :: Game -> IO()
iterateGame game = do 
            getLine
            clearCLI
            putStrLn "Press enter to continue.."
            printGrid grid 
            iterateGame game'
    where
        (Game player players grid) = game
        move = stgyAggressive player grid
        grid' = makeMove grid move
        game' = nextTurn (Game player players grid')
