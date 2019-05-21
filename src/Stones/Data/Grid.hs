{-# LANGUAGE DeriveFunctor, ScopedTypeVariables  #-}
module Stones.Data.Grid where
import           Data.List
import           Stones.Data.XY
import           Stones.Util

-- The Grid is a 2-dimensional data structure that holds values
--   The x and y coordinate indicate the second and first index respectively.
data Grid a = Grid [[a]] deriving (Show, Eq, Functor)

inBounds :: Grid a -> XY -> Bool
inBounds (Grid cells) (XY x y) =
    x >= 0 && y >= 0 && (length cells) > y && (length (cells !! y)) > x

getXY :: Grid a -> XY -> a
getXY (Grid cells) (XY x y) = cells !! y !! x

getHeight :: Grid a -> Int
getHeight (Grid cells) = length cells

getWidth :: Grid a -> Int
getWidth (Grid []) = 0
getWidth (Grid cells) = length $ cells !! 0

safeGetXY :: Grid a -> XY -> Maybe a
safeGetXY grid xy | inBounds grid xy = Just (getXY grid xy)
                  | otherwise        = Nothing

setXY :: Grid a -> a -> XY -> Grid a
setXY (Grid cells) newCell (XY x y) = (Grid newCells)
  where
    newCells = setAt cells y newRow
    newRow   = setAt (cells !! y) x newCell

mapGridXY :: (XY -> a -> b) -> Grid a -> Grid b
mapGridXY f (Grid cells) = Grid (mapWithIndex2D f' cells)
    where f' = \y x a -> f (XY x y) a

listWithXY :: Grid a -> [(XY, a)]
listWithXY grid = values
  where
    (Grid cells') = mapGridXY (\xy a -> (xy, a)) grid
    values        = concat cells'
