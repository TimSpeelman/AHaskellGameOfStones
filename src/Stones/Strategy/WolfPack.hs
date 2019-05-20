module Stones.Strategy.WolfPack 
    where
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Data.List
import Stones.Util
import Stones.Data.Pathfinding
import Data.Maybe

stgyWolfPack :: Player -> StoneGrid -> Maybe Move
stgyWolfPack player grid = move
    where
        myPositions = map fst $ stonesOfPlayer player grid
        theirPositions = map fst $ stonesOfEnemyOf player grid

        -- Each stone picks the enemy closest to it
        pairs :: [(XY, XY)]
        pairs = map (\x -> (x, closestTo x theirPositions)) myPositions

        -- For each pair, we determine the next step (if possible)
        moves = catMaybes $ map (\(a,b) -> movesToward myPositions a b) pairs

        -- The farthest attacks
        dist (a, _, b) = distanceSquared a b
        farthest :: Maybe (XY, XY, XY)
        farthest = maxSuchThat dist moves

        -- Construct the move, if available
        toMove (myPos, nextPos, _) = Move myPos nextPos
        move = fmap toMove farthest

-- Move from a to b, omitting a list of XYs
movesToward :: [XY] -> XY -> XY -> Maybe (XY, XY, XY)
movesToward omits a b = fmap (\x -> (a, x, b)) c
    where
        c :: Maybe XY
        c = stepTowardsOmitting omits a b
