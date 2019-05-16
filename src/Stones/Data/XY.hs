module Stones.Data.XY where

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
