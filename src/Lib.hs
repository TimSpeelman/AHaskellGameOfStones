module Lib
    where
import Stones.Laws
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Stones.UI.CLI
import Stones.Data.Game
import Stones.Strategy.Aggressive
import Stones.Strategy.WolfPack

dummyGame :: Game
dummyGame = Game firstPlayer players grid
    where       
        grid = gridFromLists [
                [1, 1, 1, 1],
                [0, 0, 0, 0],
                [0, 0, 0, 0],
                [0, 0, 0, 0],
                [2, 2, 2, 2]
                ]
        players = listPlayers grid
        firstPlayer = players !! 0

demoGame :: IO ()
demoGame = do
    clearCLI
    putStrLn "Welcome to a Haskell Game of Stones, press enter.."
    showGame dummyGame

showGame :: Game -> IO()
showGame game@(Game player players grid) = do
    printGrid grid
    putStrLn "Press enter to continue.."
    getLine
    verifyWinner game

verifyWinner :: Game -> IO()
verifyWinner game 
    | win /= Nothing = putStrLn $ "We have a winner! " ++ (show win)
    | otherwise = askNextMove game
    where
        win = winner game

askNextMove :: Game -> IO()
askNextMove game@(Game player players grid)
    | move == Nothing = putStrLn $ (show player) ++ " seems to be out of options.."
    | isLawful grid player (unp move) = makeNextMove game (unp move)
    | otherwise = putStrLn $ "Treason! " ++ (show player) ++ " shall hang for this"
    where 
        -- TODO: Make this strategy a parameter per player
        -- move = stgyWolfPack player grid
        move = stgyAggressive player grid
        unp (Just x) = x

makeNextMove :: Game -> Move -> IO()
makeNextMove game@(Game player players grid) move = do
    clearCLI
    putStrLn ( (show player) ++ ": " ++ (show move) )
    showGame game'
    where 
        grid' = makeMove grid move
        game' = nextTurn (Game player players grid')
