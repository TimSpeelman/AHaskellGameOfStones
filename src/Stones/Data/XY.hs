module Stones.Data.XY
   where

data XY = XY Int Int deriving (Show, Eq)

offset :: XY -> XY -> XY
offset (XY x0 y0) (XY x1 y1) = XY dx dy
    where
        dx = x1 - x0
        dy = y1 - y0
    
distanceSquared :: XY -> XY -> Int
distanceSquared xy0 xy1 = dx ^ 2 + dy ^ 2
    where
        (XY dx dy) = offset xy0 xy1
