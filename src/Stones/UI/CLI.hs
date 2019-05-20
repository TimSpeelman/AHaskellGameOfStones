module Stones.UI.CLI
    where   
import Stones.Util
import Stones.Data.Grid
import Stones.Data.StoneGrid
import System.Console.ANSI

printGrid :: StoneGrid -> IO()
printGrid grid = 
    putStrLn (gridToStr grid)
    

clearCLI :: IO()
clearCLI =
    clearScreen

gridToStr :: StoneGrid -> String
gridToStr (Grid cells) = text
    where
        texts = (fmap . fmap) cellToStr cells
        lines = fmap (joinStr " ") texts
        text = joinStr "\n" lines

cellToStr :: CellV -> String
cellToStr EmptyCell = "-"
cellToStr (CellS (Stone x)) = show x
