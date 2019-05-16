module Stones.Strategy.Aggressive 
    where
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Data.List
import Stones.Util

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
quickestTo sourcePos@(XY x0 y0) targetPos = nextPos
    where
        d = signum (targetPos - sourcePos)
        nextPos = sourcePos + d

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
        pairs = allPairs as bs
        pair = minSuchThat dist pairs

minSuchThat :: (Eq a, Ord b) => (a -> b) -> [a] -> a
minSuchThat f as = a
    where
        ords = map f as
        min = minimum ords
        (Just a) = find (\x -> (f x) == min) as
