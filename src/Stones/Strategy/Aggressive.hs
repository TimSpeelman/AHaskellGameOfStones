module Stones.Strategy.Aggressive 
    (
        stgyAggressive
    )
    where
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Data.List

stgyAggressive :: Player -> StoneGrid -> Move
stgyAggressive player grid = move
    where
        myStones = stonesOfPlayer player grid
        theirStones = stonesOfEnemyOf player grid
        pair = closestPair myStones theirStones
        ((myPos, _), (theirPos, _)) = pair
        nextPos = quickestTo myPos theirPos
        move = Move myPos nextPos
        
quickestTo :: XY -> XY -> XY
quickestTo sourcePos@(XY x0 y0) targetPos = (XY x1 y1)
    where -- UGLY!!
        (XY dx dy) = offset sourcePos targetPos
        dx' = limitToRange 1 dx
        dy' = limitToRange 1 dy
        x1 = x0 + dx'
        y1 = y0 + dy'

limitToRange :: Int -> Int -> Int
limitToRange range a
  | a > 0 = min a range
  | a <= 0 = max a (-range)

stonesOfPlayer :: Player -> StoneGrid -> [StoneXY]
stonesOfPlayer (Player player) grid = stones
    where
        allStones :: [(XY, Stone)]
        allStones = listStones grid
        isMine = \(xy, (Stone p)) -> p == player
        stones = filter isMine allStones

stonesOfEnemyOf :: Player -> StoneGrid -> [StoneXY]
stonesOfEnemyOf (Player player) grid = stones
    where
        allStones :: [(XY, Stone)]
        allStones = listStones grid
        isEnemy = \(xy, (Stone p)) -> p /= player
        stones = filter isEnemy allStones

closestPair :: [StoneXY] -> [StoneXY] -> (StoneXY, StoneXY)
closestPair as bs = pair
    where
        dist :: (StoneXY, StoneXY) -> Int
        dist ((a, _), (b, _)) = distanceSquared a b
        pairs = zip as bs
        pair = minSuchThat dist pairs

minSuchThat :: (Eq a, Ord b) => (a -> b) -> [a] -> a
minSuchThat f as = a
    where
        ords = map f as
        min = minimum ords
        (Just a) = find (\x -> (f x) == min) as
