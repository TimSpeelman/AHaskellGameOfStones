module Stones.Laws
    (
        isLawful
    ) where
        
import Stones.Data.Grid
import Stones.Data.StoneGrid
import Stones.Data.XY
import Stones.Data.Move

isLawful :: StoneGrid -> Player -> Move -> Bool
isLawful grid player move =
    thyShallTakeButOneStep move &&
    thyShallMoveButThyself grid player move &&
    thyShallNotWander grid move &&
    thyShallHitButThyFoe grid move

thyShallTakeButOneStep :: Move -> Bool
thyShallTakeButOneStep (Move sourcePos targetPos) =
    d > 0 && d <= 2
    where 
        d = distanceSquared sourcePos targetPos
        
thyShallMoveButThyself :: StoneGrid -> Player -> Move -> Bool
thyShallMoveButThyself grid (Player playerId) (Move sourcePos _) =
    getPlayerXY grid sourcePos == Just playerId

thyShallNotWander :: StoneGrid -> Move -> Bool
thyShallNotWander grid (Move sourcePos targetPos) =
    inBounds grid targetPos

thyShallHitButThyFoe :: StoneGrid -> Move -> Bool
thyShallHitButThyFoe grid (Move sourcePos targetPos) =
    fromPlayer /= toPlayer
    where
        fromPlayer = getPlayerXY grid sourcePos    
        toPlayer = getPlayerXY grid targetPos    
        