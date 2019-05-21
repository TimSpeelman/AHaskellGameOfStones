module Stones.Data.Game
    where
--  (
--         Game,
--         getCurrentPlayerIndex,
--         countPlayers,
--         getPlayer,
--         getPlayerList,
--         getGrid,
--         nextTurn,
--         winner
--     ) where 
import Stones.Data.StoneGrid
import Data.List
import Stones.Data.Move
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive
-- data Game = Game Player [Player] StoneGrid deriving (Eq, Show)   

data Game = Game {
        getPlayer :: Player,
        getPlayerList :: [Player],
        getGrid :: StoneGrid
    } deriving (Eq, Show)  
    
getNextMove :: Game -> Maybe Move
getNextMove game = pickStrategy (getPlayer game) (getGrid game)

makeGameMove :: Game -> Move -> Game
makeGameMove game move = game'
    where    
        player = getPlayer game
        grid = getGrid game
        grid' = makeMove grid move
        game' = nextTurn $ game { getGrid = grid' }

-- Temporary solution: replace this by the user selecting the strategies in the CLI
pickStrategy :: Player -> StoneGrid -> Maybe Move
pickStrategy p@(Player 2) = stgyNonSuicidalAggressive p
pickStrategy p@(Player 1) = stgyNonSuicidalAggressive p

getCurrentPlayerIndex :: Game -> Maybe Int
getCurrentPlayerIndex game = findIndex (\p -> p == getPlayer game) (getPlayerList game)

countPlayers :: Game -> Int
countPlayers game = length $ getPlayerList game

nextTurn :: Game -> Game
nextTurn game = game { getPlayer = nextPlayer }
    where
        (Just currIndex) = getCurrentPlayerIndex game
        nextIndex = (currIndex + 1) `mod` (countPlayers game)
        nextPlayer = (getPlayerList game) !! nextIndex

-- nextTurn :: Game -> Game
-- nextTurn (Game player players grid) = game'
--     where
--         (Just currIndex) = findIndex (\p -> p == player) players
--         nextIndex = (currIndex + 1) `mod` (length players)
--         nextPlayer = players !! nextIndex
--         game' = Game nextPlayer players grid
        
-- winner :: Game -> Maybe Player
-- winner (Game _ _ grid) = w
--     where
--         players = listPlayers grid
--         w = if (length players) == 1
--                 then Just $ players !! 0
--                 else Nothing
        
winner :: Game -> Maybe Player
winner game 
  | countPlayers game == 1 = Just $ (getPlayerList game) !! 0
  | otherwise = Nothing
                