-- This module allows you to run a Haskell Game of Stones
-- interactively on a command-line interface. By hitting
-- enter you can walk through the game step-by-step.
module Stones.CLI
    where   
import System.Console.ANSI ( clearScreen )
import System.IO
import Stones.Data.Game
import Stones.TextRender

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
