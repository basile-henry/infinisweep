# HaskellSweeper
HaskellSweeper is a clone of the famous [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_%28video_game%29) game written in Haskell. If features an infinit grid which means that a game could (in theory) go on forever.

This game is played in a terminal using ncurses to render it.

## How to build
The building process should be pretty straight forward. Using the cabal build file it goes as follows:
```bash
cabal install c2hs alex language-c ncurses
cabal configure
cabal build
```

## How to run the game
```bash
./HaskellSweeper [options]
```

With these `options`:
-`auto` -- When placing markers, the cells that are satisfied (number in the cell corresponds to the number of markers around it) will be opened automagically!
-`adventure` -- Play in adventure mode (coming soon)

## How to play
When a cell in the grid is opened it either contains a mine and therefore explodes (Game Over) or will show the player the number of mines in the neighbouring cells (there are 8 neighbouring cells).
- To move around the grid use the arrow keys (←, ↑, ↓, →).
- Press space (' ') to open a cell.
- Press M to mark a cell (if you think it contains a mine).
- Press Q to quit the game.

