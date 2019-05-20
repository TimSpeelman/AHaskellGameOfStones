module Stones.Strategy.Aggressive 
    where
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Data.List
import Stones.Util
import Stones.Data.Pathfinding

stgyAggressive :: Player -> StoneGrid -> Maybe Move
stgyAggressive player grid = move
    where
        myPositions = map fst $ stonesOfPlayer player grid
        theirPositions = map fst $ stonesOfEnemyOf player grid
        pair = closestPair $ allPairs myPositions theirPositions
        (myPos, theirPos) = pair
        nextPos = stepTowards myPos theirPos
        move = Just $ Move myPos nextPos
