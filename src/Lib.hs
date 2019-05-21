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
dummyGame = Game firstPlayer players grid
    where       
        grid = multiRowDuel 3 $ gridWH 10 10
        players = listPlayers grid
        firstPlayer = players !! 0

(Game p ps g) = dummyGame

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
        move = pickStrategy player grid
        unp (Just x) = x

-- Temporary solution: replace this by the user selecting the strategies in the CLI
pickStrategy :: Player -> StoneGrid -> Maybe Move
pickStrategy p@(Player 2) = stgyNonSuicidalAggressive p
pickStrategy p@(Player 1) = stgyNonSuicidalAggressive p


makeNextMove :: Game -> Move -> IO()
makeNextMove game@(Game player players grid) move = do
    clearCLI
    putStrLn ( (show player) ++ ": " ++ (show move) )
    showGame game'
    where 
        grid' = makeMove grid move
        game' = nextTurn (Game player players grid')
