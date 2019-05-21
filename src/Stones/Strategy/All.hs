module Stones.Strategy.All
    where
import Stones.Strategy.Aggressive
import Stones.Strategy.NonSuicidalAggressive
import Stones.Strategy.WolfPack

strategies = [
    stgyNonSuicidalAggressive,
    stgyAggressive
    ]
        