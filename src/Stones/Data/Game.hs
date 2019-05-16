module Stones.Data.Game
    where
import Stones.Data.StoneGrid
import Data.List

data Game = Game Player [Player] StoneGrid
    deriving (Eq, Show)

nextTurn :: Game -> Game
nextTurn (Game player players grid) = game'
    where
        (Just currIndex) = findIndex (\p -> p == player) players
        nextIndex = (currIndex + 1) `mod` (length players)
        nextPlayer = players !! nextIndex
        game' = Game nextPlayer players grid
        
winner :: Game -> Maybe Player
winner (Game _ _ grid) = w
    where
        players = listPlayers grid
        w = if (length players) == 1
                then Just $ players !! 0
                else Nothing
