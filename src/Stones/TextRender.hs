module Stones.TextRender
    where   
import Stones.Util
import Stones.Data.Grid
import Stones.Data.Move
import Stones.Data.Game
import Stones.Data.StoneGrid
import System.Console.Pretty 

drawGame :: Game -> String
drawGame game =
    ( color Red $ "Round " ++ ( show $ getRound game ) )
    ++ "\n\n" ++ 
    ( foldMap (\x -> x ++ "\n") ( moveToStr <$> (take 5 . reverse $ getMoves game) ) )
    ++ "\n" ++
    ( gridToStr $ getGrid game )
    ++ "\n\n" ++ 
    ( getGameStatus game )

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

getGameStatus :: Game -> String
getGameStatus game = case (getStatus game) of
    (GamePlaying) -> "Press enter to continue.."
    (GameFoul v) -> "Foul play! " ++ (show v)
    (GameWon win) -> "We have a winner! " ++ (show win)
    