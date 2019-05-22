-- This module allows you to run a Haskell Game of Stones
-- interactively on a command-line interface. By hitting
-- enter you can walk through the game step-by-step.
module Stones.CLI
    where   
import System.Console.ANSI ( clearScreen )
import System.IO
import Stones.Data.Game
import Stones.TextRender
import Stones.Data.Strategy
import Stones.Strategy.All
import Stones.Scenario.GenerateGames

createInteractiveGameOnCLI :: (Int -> Int -> Int -> Game) -> IO()
createInteractiveGameOnCLI gameFac = do
    putStrLn "Welcome to A Haskell Game of Stones!"
    putStr "How wide will thee ride? "
    w <- getLine
    putStr "How high will thee fly?  "
    h <- getLine
    putStr "How many rows to go?     "
    r <- getLine

    putStrLn "Which strategies will rule?"
    printStrategies
    putStr "Enter the number for player 1: "
    s1 <- getLine
    putStr "Enter the number for player 2: "
    s2 <- getLine

    runInteractiveGameOnCLI $ game w h r s1 s2
    where
        game w h r s1 s2 = multiRowDuelGame (read w :: Int) (read h :: Int) (read r :: Int) (read s1 :: Int) (read s2 :: Int)

printStrategies :: IO()
printStrategies = 
    putStrLn $ foldMap (\(i, s) -> (show i) ++ ": " ++ (getName s) ++ "\n") stgys
    where
        stgys = zip [0..] strategies

runInteractiveGameOnCLI :: Game -> IO()
runInteractiveGameOnCLI game = do
    -- We need to buffer otherwise the CLI gets jiggy
    hSetBuffering stdout (BlockBuffering Nothing)
    clearCLI
    putStrLn $ drawGame game
    hFlush stdout -- Flush it!
    getLine -- Await user input
    nextGameStep game

nextGameStep :: Game -> IO()
nextGameStep game = case (getStatus game) of
    (GamePlaying) -> runInteractiveGameOnCLI $ nextGameIteration game
    (GameFoul _) -> return ()
    (GameWon _) -> return () 

clearCLI :: IO()
clearCLI =
    clearScreen
