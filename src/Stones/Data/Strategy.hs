module Stones.Data.Strategy 
    where

import Stones.Data.StoneGrid
import Stones.Data.Move

data StrategyDesc = Strategy {
        getName :: String,
        getDescription :: String,
        getFunction :: StrategyFunction
    } deriving Show

data StrategyFunction = StgyFun (Player -> StoneGrid -> Maybe Move)

instance Show StrategyFunction where
    show _ = "<StgyFun>"
    