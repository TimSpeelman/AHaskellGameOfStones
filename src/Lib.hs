{-# LANGUAGE OverloadedStrings #-}
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

import System.IO
import System.Console.Pretty 

-- Temporary solution: replace this by the user selecting the strategies in the CLI
pickStrategy :: Player -> StoneGrid -> Maybe Move
pickStrategy p@(Player 2) = stgyNonSuicidalAggressive p
pickStrategy p@(Player 1) = stgyNonSuicidalAggressive p


dummyGame :: Int -> Int -> Int ->Game
dummyGame w h numRows  = newGame players grid (StrategyPicker pickStrategy)
    where       
        grid = multiRowDuel numRows $ gridWH w h 
        players = listPlayers grid

p = getPlayer $ dummyGame 10 10 2
g = getGrid $ dummyGame 10 10 2

demoGame :: Int -> Int -> Int -> IO ()
demoGame w h numRows = do
    hSetBuffering stdout $ BlockBuffering Nothing
    clearCLI
    putStrLn "Welcome to a Haskell Game of Stones, press enter.."
    showGame $ dummyGame w h numRows

showGame :: Game -> IO()
showGame game = do
    clearCLI
    putStrLn $ ( color Red $ "Round " ++ ( show $ getRound game ) )
    putStrLn ""
    foldMap putStrLn ( moveToStr <$> (take 5 . reverse $ getMoves game) )
    putStrLn ""
    printGrid $ getGrid game
    putStrLn ""
    putStrLn $ getGameStatus game
    hFlush stdout
    getLine
    nextGameStep game

nextGameStep :: Game -> IO()
nextGameStep game = case (getStatus game) of
    (GamePlaying) -> showGame $ nextGameIteration game
    (GameFoul _) -> return ()
    (GameWon _) -> return () 

getGameStatus :: Game -> String
getGameStatus game = case (getStatus game) of
    (GamePlaying) -> "Press enter to continue.."
    (GameFoul v) -> "Foul play! " ++ (show v)
    (GameWon win) -> "We have a winner! " ++ (show win)
    
