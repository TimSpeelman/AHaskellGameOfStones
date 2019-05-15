module Lib
    ( demoGame
    ) where
import Stones.Laws
import Stones.Data.StoneGrid
import Stones.UI.CLI

demoGame :: IO ()
demoGame = printGrid grid
    where
        grid = gridFromLists [
            [1, 1, 1, 1],
            [0, 0, 0, 0],
            [2, 2, 2, 2]
            ]
