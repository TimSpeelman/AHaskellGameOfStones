{-#  LANGUAGE TypeSynonymInstances #-}
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
import Stones.Laws
import Stones.Data.Move
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive
-- data Game = Game Player [Player] StoneGrid deriving (Eq, Show)   


data StrategyPicker = StrategyPicker (Player -> StoneGrid -> Maybe Move) 
instance Show StrategyPicker where
    show s = "StrategyPicker"

data Game = Game {
        getPlayer :: Player,
        getPlayerList :: [Player],
        getGrid :: StoneGrid,
        getStatus :: GameStatus,
        getMoves :: [(Player, Move)],
        getStrategy :: StrategyPicker -- Player -> StoneGrid -> Maybe Move
    } deriving (Show)  

data GameStatus = 
    GamePlaying
    | GameFoul Violation
    | GameWon Player deriving (Eq, Show)
    
newGame :: [Player] -> StoneGrid -> Game
newGame players grid = Game {
        getPlayer = players !! 0,
        getPlayerList = players,
        getGrid = grid,
        getStatus = GamePlaying,
        getStrategy = StrategyPicker (pickStrategy),
        getMoves = []
    }

nextGameIteration :: Game -> Game
nextGameIteration g = case (getStatus g) of
    GamePlaying -> submitMove g $ getNextMove g
    _ -> g
    
-- Based on the player, calculate the next move according to its strategy
getNextMove :: Game -> Maybe Move
getNextMove game = strategy (getGrid game)
    where
        StrategyPicker pickStgy = getStrategy game
        strategy = pickStgy (getPlayer game) 

submitMove :: Game -> Maybe Move -> Game
submitMove game move = case violation of
    Nothing -> performMove game $ unp move
    (Just vial) -> quitByViolation game vial
  where
    violation = validateLawful (getGrid game) (getPlayer game) move
    unp (Just x) = x

quitByViolation :: Game -> Violation -> Game
quitByViolation g v = g { getStatus = GameFoul v }

performMove :: Game -> Move -> Game
performMove game move = finishTurn game'
    where    
        player = getPlayer game
        grid = getGrid game
        grid' = makeMove grid move
        moves = getMoves game
        game' = game { getGrid = grid', getMoves = moves ++ [(player, move)] }

finishTurn :: Game -> Game 
finishTurn = updateStatus . nextTurn

-- appendMove :: Game -> Player -> Move -> Game
-- appendMove game player move = game { getMoves = moves }
--     where
--         moves' = (getMoves game)
--         moves = moves ++ [(player, move)]

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
winner game = case (getStatus game) of
    GameWon p -> Just p
    GamePlaying -> Nothing

updateStatus :: Game -> Game
updateStatus game = case lastManStanding game of
    Nothing -> game
    Just p -> game { getStatus = GameWon p }
  
lastManStanding :: Game -> Maybe Player
lastManStanding game
  | length p > 1 = Nothing
  | otherwise = Just $ (p !! 0)
    where 
        p = listPlayers $ getGrid game
