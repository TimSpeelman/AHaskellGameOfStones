# AHaskellGameOfStones
A Haskell Game of Stones

## How the game works
A Haskell Game of Stones is a simple turn-based game with a 2D board of `n by m` tiles. Each player starts on the board with `k` stones. During his turn, the player can move one of his stones to an adjacent tile (straight or diagonal) if it is free, or taken by an enemy stone. If it is occupied by an enemy stone, this stone is removed from the board. The last player standing wins.

An example 3-by-4 game with two players starts out like this:
```
1 1 1 1 
- - - -
2 2 2 2
```

The game is not player by humans, but by 'bots' operating in the form of a strategy. An example strategy is [Aggressive](./src/Stones/Strategy/Aggressive.hs) which finds the nearest enemy stone and moves towards that.

The objective for us humans is to code the best strategy.

## Running the game
Use `stack build --exec AHaskellGameOfStones-exe` to run the game.

## Command-Line Interface
A graphical user interface has not been implemented so far, so we will have to do with a CLI.
