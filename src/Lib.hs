module Lib
    where
import Stones.Laws
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Stones.UI.CLI
import Stones.Data.Game
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive
import Stones.Strategy.WolfPack
import Stones.Scenario.GenerateGames

dummyGame :: Game
dummyGame = Game { 
        getPlayer = firstPlayer,
        getPlayerList = players,
        getGrid = grid
    }
    where       
        grid = multiRowDuel 2 $ gridWH 3 6
        players = listPlayers grid
        firstPlayer = players !! 0

p = getPlayer dummyGame
g = getGrid dummyGame

demoGame :: IO ()
demoGame = do
    clearCLI
    putStrLn "Welcome to a Haskell Game of Stones, press enter.."
    showGame dummyGame

showGame :: Game -> IO()
showGame game = do
    printGrid $ getGrid game
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
    | ( move == Nothing ) = putStrLn $ (show player) ++ " seems to be out of options.."
    | ( isLawful grid player (unp move) ) = makeNextMove game (unp move)
    | ( otherwise ) = putStrLn $ "Treason! " ++ (show player) ++ " shall hang for this"
    where 
        player = getPlayer game
        grid = getGrid game
        move = getNextMove game
        unp (Just x) = x

makeNextMove :: Game -> Move -> IO()
makeNextMove game move = do
    clearCLI
    putStrLn ( (show player) ++ ": " ++ (show move) )
    showGame game'
    where 
        player = getPlayer game
        game' = makeGameMove game move
