{-# LANGUAGE OverloadedStrings #-}
module Lib
    where
import Stones.Laws
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Stones.CLI
import Stones.Data.Game
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive
import Stones.Strategy.WolfPack
import Stones.Strategy.All
import Stones.Scenario.GenerateGames

import System.IO
import System.Console.Pretty 

-- Temporary solution: replace this by the user selecting the strategies in the CLI
pickStrategy :: Player -> StoneGrid -> Maybe Move
pickStrategy p@(Player 2) = stgyNonSuicidalAggressiveFun p
pickStrategy p@(Player 1) = stgyNonSuicidalAggressiveFun p

dummyGame :: Int -> Int -> Int ->Game
dummyGame w h numRows  = newGame players grid (StrategyPicker pickStrategy)
    where       
        grid = multiRowDuel numRows $ gridWH w h 
        players = listPlayers grid

demoGame :: IO ()
demoGame = do
    createInteractiveGameOnCLI dummyGame
