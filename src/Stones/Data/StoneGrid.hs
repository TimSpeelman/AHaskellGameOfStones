{-# LANGUAGE DeriveFunctor, ScopedTypeVariables  #-}
module Stones.Data.StoneGrid
    where
import Data.List
import Stones.Data.Grid
import Stones.Data.XY
import Stones.Util

type StoneGrid = Grid CellV
type StoneXY = (XY, Stone)

-- The stone has a player id
data Stone = Stone Int
    deriving (Show, Eq)


data Player = Player Int deriving (Show, Eq)

-- This could be a Maybe, but maybe we want to expand this with
--   more than just stones later.
data CellV =
    EmptyCell 
    | CellS Stone
    deriving (Show, Eq)

gridWH :: Int -> Int -> StoneGrid
gridWH numCols numRows = Grid cells
    where
        cells = replicate numRows (replicate numCols EmptyCell)

gridFromLists :: [[Int]] -> StoneGrid
gridFromLists values = Grid cells
    where
        cells = (fmap . fmap) cellFromInt values

cellFromInt :: Int -> CellV
cellFromInt 0 = EmptyCell
cellFromInt x = CellS (Stone x)

setPlayerXY :: StoneGrid -> Int -> XY -> StoneGrid
setPlayerXY grid playerId xy =
    setXY grid (CellS (Stone playerId)) xy

getPlayerXY :: StoneGrid -> XY -> Maybe Int
getPlayerXY grid xy = playerFromCell (getXY grid xy)

playerFromCell :: CellV -> Maybe Int
playerFromCell EmptyCell = Nothing
playerFromCell (CellS (Stone id)) = Just id

clearCell :: StoneGrid -> XY -> StoneGrid
clearCell grid xy =
    setXY grid EmptyCell xy

listStones :: StoneGrid -> [StoneXY]
listStones grid = stones
    where
        elems :: [(XY, CellV)]
        elems = listWithXY grid
        isStone :: (XY, CellV) -> Bool
        isStone (_, (CellS _)) = True
        isStone (_, _) = False
        stones' = filter isStone elems
        stones = map (\(xy, (CellS s)) -> (xy, s)) stones'

listPlayers :: StoneGrid -> [Player]
listPlayers grid = players
    where
        players = unique $ map toPlayer (listStones grid)
        toPlayer :: StoneXY -> Player
        toPlayer (_, Stone p) = (Player p)  
        unique = reverse . nub . reverse
