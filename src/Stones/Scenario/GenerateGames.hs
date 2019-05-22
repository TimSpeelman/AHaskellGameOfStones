module Stones.Scenario.GenerateGames
    where

import Stones.Data.StoneGrid
import Stones.Data.Grid
import Stones.Data.XY
import Stones.Data.Game
import Stones.Strategy.All
import Stones.Data.Strategy

multiRowDuel :: Int -> StoneGrid -> StoneGrid
multiRowDuel numRows grid = filledGrid
    where
        filledGrid = mapGridXY f grid
        height = getHeight grid 
        f (XY x y) _
          | y < numRows = CellS (Stone 1)
          | y >= height - numRows = CellS (Stone 2)
          | otherwise = EmptyCell

multiRowDuelGame :: Int -> Int -> Int -> Int -> Int ->Game
multiRowDuelGame w h numRows s1 s2 = newGame players grid stgyPick
    where       
        grid = multiRowDuel numRows $ gridWH w h 
        players = listPlayers grid
        -- Hacky:
        stgyPick = makeStgyPicker [
                getFunction $ strategies !! s1,
                getFunction $ strategies !! s2
            ]