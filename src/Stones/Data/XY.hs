module Stones.Data.XY where
import Stones.Util
-- This needs vector arithmetic! Don't implement by yourself!

data XY = XY Int Int deriving (Show, Eq)

instance Num XY where
    (+) (XY x y) (XY x' y') = (XY (x + x') (y + y'))
    (-) (XY x y) (XY x' y') = (XY (x - x') (y - y'))
    (*) (XY x y) (XY x' y') = (XY (x * x') (y * y'))
    negate (XY x y) = (XY (-x) (-y))
    abs (XY x y) = (XY (abs x) (abs y))
    signum (XY x y) = (XY (signum x) (signum y))
    -- Required for Num class but doesn't really make sense?
    fromInteger i = (XY i' i') where i' = fromInteger i

distanceSquared :: XY -> XY -> Int
distanceSquared a b = dx ^ 2 + dy ^ 2 
    where (XY dx dy) = b - a

neighbors :: XY -> [XY]
neighbors p = [
        p + (XY (-1) (-1)),
        p + (XY (-1) (0)),
        p + (XY (-1) (1)),
        p + (XY (0) (-1)),
        p + (XY (0) (1)),
        p + (XY (1) (-1)),
        p + (XY (1) (0)),
        p + (XY (1) (1))
    ]

-- Pair of points that is closest to each other
closestPair :: [(XY, XY)] -> (XY, XY)
closestPair pairs = minSuchThat dist pairs
    where
        dist (a, b) = distanceSquared a b

closestTo :: XY -> [XY] -> XY
closestTo a bs = b
    where
        (_, b) = closestPair $ allPairs [a] bs
