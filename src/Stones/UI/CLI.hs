module Stones.UI.CLI
    where   
import Stones.Util
import Stones.Data.Grid
import Stones.Data.StoneGrid

printGrid :: StoneGrid -> IO()
printGrid grid = 
    putStrLn (gridToStr grid)
    
gridToStr :: StoneGrid -> String
gridToStr (Grid cells) = text
    where
        texts = (fmap . fmap) cellToStr cells
        lines = fmap (join " ") texts
        text = join "\n" lines

cellToStr :: CellV -> String
cellToStr EmptyCell = "-"
cellToStr (CellS (Stone x)) = show x
