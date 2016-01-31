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

## How to play

