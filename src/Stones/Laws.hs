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
    thouShallTakeButOneStep move &&
    thouShallMoveButThyself grid player move &&
    thouShallNotWander grid move &&
    thouShallHitButThyFoe grid move

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
        