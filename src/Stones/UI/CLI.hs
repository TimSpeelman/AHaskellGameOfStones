module Stones.UI.CLI
    where   
import Stones.Util
import Stones.Data.Grid
import Stones.Data.Move
import Stones.Data.StoneGrid
import System.Console.ANSI ( clearScreen )
import System.Console.Pretty 

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
cellToStr (CellS (Stone x)) = color c $ format x
    where
        c = playerColor (Player x)
        format :: Int -> String
        format 1 = "X"
        format 2 = "O"
        format x = (show x) :: String

moveToStr :: (Player, Move) -> String
moveToStr (player, move) = color (playerColor player) $ (show player) ++ ": " ++ (show move)
        
playerColor :: Player -> Color
playerColor (Player 1) = Red
playerColor (Player 2) = Blue
playerColor _ = Yellow