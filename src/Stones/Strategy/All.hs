module Stones.Strategy.All
    where
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive
import Stones.Strategy.WolfPack
import Stones.Data.Game
import Stones.Data.Strategy
import Stones.Data.StoneGrid

strategies :: [StrategyDesc]
strategies = [
    stgyNonSuicidalAggressive,
    stgyAggressive
    ]

makeStgyPicker :: [StrategyFunction] -> StrategyPicker
makeStgyPicker stgys = (StrategyPicker f)
    where
         -- zero indexing, hacky!
         -- pass player
        -- f :: Player -> StoneGrid -> Maybe Move
        f plyr@(Player x) = ( chooseFun x ) plyr
        chooseFun index = case ( stgys !! (index - 1) ) of
            (StgyFun f) -> f