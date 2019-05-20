import Test.Hspec
import Stones.Data.StoneGrid
import Stones.Data.XY
import Stones.Data.Grid
import Stones.Laws
import Stones.Data.Move
import Stones.Strategy.WolfPack

main :: IO ()
main = hspec $ do
    (
        describe "WolfPack" $ do
        ( it "movesToward" $ do 
            movesToward' [] (XY 0 0) (XY 2 0) `shouldBe` Just (XY 1 0)
            
            )
        )
    ( describe "Make Grid" $ do
        ( it "makes an empty 1x2 grid" $ do
            gridWH 2 1 `shouldBe` Grid [
                    [EmptyCell, EmptyCell]
                ] )
        ( it "makes an empty 2x1 grid" $ do
            gridWH 1 2 `shouldBe` Grid [
                    [EmptyCell],
                    [EmptyCell]
                ] )
        ( it "makes a filled grid" $ do
            gridFromLists [[1, 2, 0]] `shouldBe` Grid [
                    [CellS (Stone 1), CellS (Stone 2), EmptyCell]
                ] )
        )
    ( describe "Access Grid" $ do 
        ( it "signals in-bounds" $ do
            inBounds empty2x1Grid (XY 0 0) `shouldBe` True
            inBounds empty2x1Grid (XY 0 1) `shouldBe` True
            )
        ( it "signals out-of-bounds" $ do
            inBounds empty2x1Grid (XY 0 (-1)) `shouldBe` False
            inBounds empty2x1Grid (XY (-1) 0) `shouldBe` False
            inBounds empty2x1Grid (XY 1 0) `shouldBe` False
            inBounds empty2x1Grid (XY 0 2) `shouldBe` False
            )
        ( it "provides safe access inside"  $ do
            safeGetXY empty2x1Grid (XY 0 0) `shouldBe` (Just (EmptyCell))
            safeGetXY empty2x1Grid (XY 0 1) `shouldBe` (Just (EmptyCell))
            )
        ( it "provides safe access outside"  $ do
            safeGetXY empty2x1Grid (XY 0 (-1)) `shouldBe` Nothing
            safeGetXY empty2x1Grid (XY (-1) 0) `shouldBe` Nothing
            safeGetXY empty2x1Grid (XY 1 0) `shouldBe` Nothing
            safeGetXY empty2x1Grid (XY 0 2) `shouldBe` Nothing
            )
        ( it "sets a new cell value"  $ do
            setPlayerXY empty2x1Grid 1 (XY 0 0) `shouldBe` (Grid [
                    [CellS (Stone 1)],
                    [EmptyCell]
                ])
            )
        ( it "clears a cell"  $ do
            clearCell grid2x1StartLeft (XY 0 0) `shouldBe` (Grid [
                    [EmptyCell], [EmptyCell]
                ])
            )
        )
    ( describe "Laws" $ do 
        ( it "satisfies thyShallTakeButOneStep"  $ do
            isLawful sampleGrid1 (Player 1) (move 1 1 0 0) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 0 1) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 0 2) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 1 0) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 1 2) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 2 0) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 2 1) `shouldBe` True
            isLawful sampleGrid1 (Player 1) (move 1 1 2 2) `shouldBe` True
            )
        ( it "rejects thyShallTakeButOneStep when farther"  $ do
            isLawful sampleGrid1 (Player 1)  (move 1 1 3 2) `shouldBe` False
            )
        ( it "rejects thyShallTakeButOneStep when same cell" $ do 
            isLawful sampleGrid1 (Player 1)  (move 1 1 1 1) `shouldBe` False
            )
        ( it "rejects thyShallHitButThyFoe" $ do 
            isLawful sampleGrid2 (Player 1)  (move 1 1 2 1) `shouldBe` False
            )
        ( it "satisfies thyShallHitButThyFoe" $ do 
            isLawful sampleGrid2 (Player 1)  (move 2 1 3 2) `shouldBe` True
            )
        ( it "rejects thyShallMoveButThyself" $ do 
            isLawful sampleGrid2 (Player 1)  (move 3 2 2 2) `shouldBe` False
            )
        ( it "rejects thyShallNotWander" $ do 
            isLawful sampleGrid2 (Player 2)  (move 3 2 4 2) `shouldBe` False
            )
        )

empty2x1Grid = (gridWH 1 2)
empty3x2Grid = (gridWH 2 3)
grid2x1StartLeft = setPlayerXY empty2x1Grid 1 (XY 0 0)

sampleGrid1 = gridFromLists [
        [0, 0, 0, 0],
        [0, 1, 0, 0],
        [0, 0, 0, 0]
    ]

sampleGrid2 = gridFromLists [
        [0, 0, 0, 0],
        [0, 1, 1, 0],
        [0, 0, 0, 2]
    ]