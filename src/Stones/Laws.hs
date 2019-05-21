module Stones.Laws
    (
        Violation, 
        isLawful,
        validateLawful
    ) where
        
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.XY
import Stones.Data.Move

data Violation = 
    NoMoveViolation
    | TakeButOneStepViolation
    | MoveButThyselfViolation
    | NotWanderViolation
    | HitButThyFoeViolation
    deriving (Eq, Show)

isLawful :: StoneGrid -> Player -> Move -> Bool
isLawful g p m = validateLawful g p (Just m) == Nothing

validateLawful :: StoneGrid -> Player -> Maybe Move -> Maybe Violation
validateLawful grid player Nothing = Just $ NoMoveViolation
validateLawful grid player (Just move)
  | not $ thouShallTakeButOneStep move = Just $ TakeButOneStepViolation
  | not $ thouShallMoveButThyself grid player move = Just $ MoveButThyselfViolation
  | not $ thouShallNotWander grid move = Just $ NotWanderViolation
  | not $ thouShallHitButThyFoe grid move = Just $ HitButThyFoeViolation
  | otherwise = Nothing

thouShallTakeButOneStep :: Move -> Bool
thouShallTakeButOneStep (Move sourcePos targetPos) =
    d > 0 && d <= 2
    where 
        d = distanceSquared sourcePos targetPos
        
thouShallMoveButThyself :: StoneGrid -> Player -> Move -> Bool
thouShallMoveButThyself grid (Player playerId) (Move sourcePos _) =
    getPlayerXY grid sourcePos == Just playerId

thouShallNotWander :: StoneGrid -> Move -> Bool
thouShallNotWander grid (Move sourcePos targetPos) =
    inBounds grid targetPos

thouShallHitButThyFoe :: StoneGrid -> Move -> Bool
thouShallHitButThyFoe grid (Move sourcePos targetPos) =
    fromPlayer /= toPlayer
    where
        fromPlayer = getPlayerXY grid sourcePos    
        toPlayer = getPlayerXY grid targetPos    
        