module Stones.Data.Move
    where

import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.XY

data Move = Move XY XY deriving (Show, Eq)

move :: Int -> Int -> Int -> Int -> Move
move x0 y0 x1 y1 = Move (XY x0 y0) (XY x1 y1)

makeMove :: StoneGrid -> Move -> StoneGrid
makeMove grid (Move sourcePos targetPos) = grid''
    where
        stone = getXY grid sourcePos 
        grid' = setXY grid stone targetPos
        grid'' = setXY grid' EmptyCell sourcePos
