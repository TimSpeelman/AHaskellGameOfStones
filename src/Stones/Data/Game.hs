module Stones.Data.Game
    where

import Data.List
import Stones.Data.StoneGrid
import Stones.Laws
import Stones.Data.Move
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive

-- The Game object holds all relevant game state
data Game = Game {
        getRound :: Int,
        getPlayer :: Player,
        getPlayerList :: [Player],
        getGrid :: StoneGrid,
        getStatus :: GameStatus,
        getMoves :: [(Player, Move)],
        getStrategy :: StrategyPicker
    } deriving (Show)  

-- We can pass a function into the game which decides the strategy a given player assumes
data StrategyPicker = StrategyPicker (Player -> StoneGrid -> Maybe Move) 
instance Show StrategyPicker where
    show s = "StrategyPicker"

data GameStatus = 
    GamePlaying
    | GameFoul Violation
    | GameWon Player deriving (Eq, Show)
    
newGame :: [Player] -> StoneGrid -> StrategyPicker -> Game
newGame players grid stgyPicker = Game {
        getRound = 0,
        getPlayer = players !! 0,
        getPlayerList = players,
        getGrid = grid,
        getStatus = GamePlaying,
        getStrategy = stgyPicker,
        getMoves = []
    }
    
-- The game only iterates when it's playing.
--   Can/should we make Game an iterable?
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

-- Submit a move, will perform when it's valid
submitMove :: Game -> Maybe Move -> Game
submitMove game move = case violation of
    Nothing -> performMove game $ unp move
    (Just vial) -> quitByViolation game vial
  where
    violation = validateLawful (getGrid game) (getPlayer game) move
    unp (Just x) = x

-- When a player violated the law, quit the game.
quitByViolation :: Game -> Violation -> Game
quitByViolation g v = g { getStatus = GameFoul v }

-- Perform a move, assuming it is lawful.
performMove :: Game -> Move -> Game
performMove game move = finishTurn game'
    where    
        player = getPlayer game
        grid = getGrid game
        grid' = makeMove grid move
        moves = getMoves game
        game' = game { getGrid = grid', getMoves = moves ++ [(player, move)] }

-- When the turn is finished, check for a winner, otherwise go to next turn.
finishTurn :: Game -> Game 
finishTurn = checkForWinner . nextTurn

-- When only one player has stones left, he has won.
checkForWinner :: Game -> Game
checkForWinner game = case lastManStanding (getGrid game) of
    Nothing -> game
    Just p -> game { getStatus = GameWon p }
 
nextTurn :: Game -> Game
nextTurn game = game { getPlayer = nextPlayer, getRound = nextRound }
    where
        (Just currIndex) = getCurrentPlayerIndex game
        nextIndex = (currIndex + 1) `mod` (countPlayers game)
        nextPlayer = (getPlayerList game) !! nextIndex
        nextRound = 1 + getRound game
        
getCurrentPlayerIndex :: Game -> Maybe Int
getCurrentPlayerIndex game = findIndex (\p -> p == getPlayer game) (getPlayerList game)

countPlayers :: Game -> Int
countPlayers game = length $ getPlayerList game
    