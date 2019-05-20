module Stones.Strategy.NonSuicidalAggressive 
    where
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.Move
import Stones.Data.XY
import Data.List
import Stones.Util
import Stones.Data.Pathfinding
import Control.Monad
import Data.Maybe
import Stones.Laws

stgyNonSuicidalAggressive :: Player -> StoneGrid -> Maybe Move
stgyNonSuicidalAggressive player grid = move
    where
        myPositions = map fst $ stonesOfPlayer player grid
        theirPositions = map fst $ stonesOfEnemyOf player grid
        pair = closestPair $ allPairs myPositions theirPositions
        (myPos, theirPos) = pair
        nextPos = stepTowards myPos theirPos
        closestNonSuicidal = fmap Just $ filter (not . (isSuicidal grid player)) (attackClosest grid player)
        kamikaze = [] -- TODO Implement 
        randomNonSuicidal = [] -- TODO Implement
        closestSuicidal = Just <$> attackClosest grid player
        alternatives :: [Maybe Move]
        alternatives = closestNonSuicidal
            ++ kamikaze
            ++ randomNonSuicidal
            ++ closestSuicidal
        lawfulAlternatives = fmap (validateLawful grid player) alternatives
        move = join $ find (\x -> x /= Nothing) lawfulAlternatives

validateLawful :: StoneGrid -> Player -> Maybe Move -> Maybe Move
validateLawful grid player Nothing = Nothing
validateLawful grid player (Just move)
    | isLawful grid player move = Just move
    | otherwise = Nothing

isHit :: StoneGrid -> Player -> Move -> Bool
isHit grid player (Move src trg) = containsEnemyOf grid player trg

isSuicidal :: StoneGrid -> Player -> Move -> Bool
isSuicidal grid player (Move src trg) = find (containsEnemyOf grid player) (neighbors trg) /= Nothing

containsEnemyOf :: StoneGrid -> Player -> XY -> Bool
containsEnemyOf grid (Player p) xy 
  | inBounds grid xy =  case getPlayerXY grid xy of     
                                        (Just p') -> p /= p'
                                        (Nothing) -> False
  | otherwise = False

-- List of every stone attacking their closest enemy
attackClosest :: StoneGrid -> Player  -> [Move]
attackClosest grid player = catMaybes moves
    where
        myPositions = map fst $ stonesOfPlayer player grid
        theirPositions = map fst $ stonesOfEnemyOf player grid
        closest :: [(XY, XY)]
        closest = fmap (\my -> (my, closestTo my theirPositions)) myPositions
        compareDist (a,b) (c,d) = compare (distanceSquared a b) (distanceSquared c d)
        closestSorted = Data.List.sortBy compareDist closest
        moves = fmap (\(a, b) -> Just (Move a $ stepTowards a b)) closestSorted
