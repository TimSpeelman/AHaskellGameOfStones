module Stones.Scenario.GenerateGames
    where

import Stones.Data.StoneGrid
import Stones.Data.Grid
import Stones.Data.XY

multiRowDuel :: Int -> StoneGrid -> StoneGrid
multiRowDuel numRows grid = filledGrid
    where
        filledGrid = mapGridXY f grid
        height = getHeight grid 
        f (XY x y) _
          | y < numRows = CellS (Stone 1)
          | y >= height - numRows = CellS (Stone 2)
          | otherwise = EmptyCell
