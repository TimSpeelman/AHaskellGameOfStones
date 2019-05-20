module Stones.Data.Pathfinding
    where
import Stones.Data.XY
import Stones.Util

stepTowards :: XY -> XY -> XY
stepTowards sourcePos@(XY x0 y0) targetPos = nextPos
    where
        d = signum (targetPos - sourcePos)
        nextPos = sourcePos + d

-- Omit certain XYs when navigating from a to b
stepTowardsOmitting :: [XY] -> XY -> XY -> Maybe XY
stepTowardsOmitting omits a b = nextPos
    where
        dist = distanceSquared a b
        -- Neighbors that are not in omits
        opts = filter (\x -> (any (\y -> y == x) omits)) (neighbors a)
        closer = filter isCloserToTarget opts
        isCloserToTarget x = (distanceSquared x b) < dist
        nextPos = if closer == [] then Nothing else Just $ closer !! 0
