{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Sweeper.Game where

import           Data.Hashable       (Hashable)
import           Data.Set            (Set, delete, empty, insert, member, size)
import           GHC.Generics        (Generic)
import qualified Options.Applicative as Opt
import           System.Random       (StdGen, randomR, split)

import           Prelude             hiding (Left, Right)

import           Sweeper.Grid

data Cell       = Empty | Mine deriving Eq

-- | Set of positions (in the grid) that are visible (opened cell)
type Visibility = Set Position
-- | Set of positions that are marked as containing a mine (wether or not it actually does)
type Markers    = Set Position
-- | The score corresponds to the sum of all the numbers showing in the visible cells
type Score      = Int
data PlayState  = Alive | Dead deriving Eq

data Options = Options
  { adventure :: Bool
  , autoOpen  :: Bool
  , density   :: Int
  } deriving (Generic, Hashable)

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> pure False -- Adventure unsupported
  <*> Opt.switch
    (Opt.short 'a' <> Opt.long "auto-open" <> Opt.help "Whether to automatically open cells known to not contain a mine")
  <*> Opt.option Opt.auto
    (Opt.short 'd' <> Opt.long "density" <> Opt.help "Density of the minefield, as a percentage" <> Opt.value 20 <> Opt.metavar "PERCENT")

prettyShow :: Options -> [String]
prettyShow opts =
  ["Adventure" | adventure opts] ++
  ["Auto Open" | autoOpen opts] ++
  ["Density: " ++ show (density opts)]

data Move       = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight -- Possible ways to move on the grid
data GameState  = GameState
    {
        grid       :: Grid Cell,
        visibility :: Visibility,
        markers    :: Markers,
        score      :: Score,
        position   :: Position,
        highscore  :: Score,
        playState  :: PlayState,
        panel      :: Panel,
        randomgen  :: StdGen,
        options    :: Options
    }

-- | Count the number of mines in the positions around a given position
tallyMines :: Grid Cell -> Position -> Int
tallyMines grid pos = length $ filter (==Mine) $ map (getCell grid) (surroundingPositions pos)

-- | Count the number of markers in the positions around a given position
tallyMarkers :: Markers -> Position -> Int
tallyMarkers markers pos = length $ filter (`member` markers) (surroundingPositions pos)

-- | Randomly generate a cell given a density
randomCell :: Int -> StdGen -> (Cell, StdGen)
randomCell density gen =
  let (n, g) = randomR (0,99) gen
  in (if n < density then Mine else Empty, g)

-- | Generate a random initial GameState
createGameState :: StdGen -> Options -> Score -> GameState
createGameState gen opts hs = let (g, g') = split gen in
  GameState
    {
        grid       = randomGrid (randomCell (density opts)) g,
        visibility = empty,
        markers    = empty,
        position   = zeroPosition,
        score      = 0,
        highscore  = hs,
        playState  = Alive,
        panel      = (movePosition (-150) (-50) zeroPosition, movePosition 150 50 zeroPosition),
        randomgen  = g',
        options    = opts
    }

newGame :: GameState -> GameState
newGame GameState{randomgen=gen, options=opts, highscore=hs} = createGameState gen opts hs

-- | Recursively open cells that are empty (limited by panel to avoid infinite recursion)
getEmptyCells :: GameState -> Position -> GameState
getEmptyCells g@GameState{grid, panel, visibility, markers, score, highscore} pos
    | not (inBounds pos panel)
      || member pos visibility
      || member pos markers    = g
    | t > 0                    = g{visibility=newVis, score=score + t, highscore=max highscore (score + t)}
    | otherwise                = foldl getEmptyCells g{visibility=newVis} (surroundingPositions pos)
    where
        t :: Int
        t = tallyMines grid pos

        newVis :: Visibility
        newVis = insert pos visibility

-- | A Cell (at a given position) is satisfied if the number of Mines around it matches the number of cells
-- Markers could still be missplaced!
isSatisfied :: GameState -> Position -> Bool
isSatisfied GameState{grid, markers} p = tallyMines grid p == tallyMarkers markers p

type GameUpdate = GameState -> Maybe GameState

-- | Change the current position on the grid
--
-- I don't know what's up with the patterns here... removing the ~'s incorrectly gives an incomplete pattern warning
makeMove :: Move -> GameUpdate
makeMove move g@GameState{grid, visibility, position, panel=(topLeft@ ~(Cartesian left top), bottomRight@ ~(Cartesian right bottom))} =
    pure newGameState{position = movePosition dx dy position}
    where
        -- deltas from a Move
        (dx, dy) = case move of
            Up        -> ( 0, -1)
            Down      -> ( 0,  1)
            Left      -> (-1,  0)
            Right     -> ( 1,  0)
            UpLeft    -> (-1, -1)
            UpRight   -> ( 1, -1)
            DownLeft  -> (-1,  1)
            DownRight -> ( 1,  1)

        newPanel :: Panel
        newPanel = (movePosition dx dy topLeft, movePosition dx dy bottomRight)

        -- cells on the edge of the panel that need to be updated because the panel is moving
        -- this update is necessary since the cells opened recursively stopped at the edge of the panel
        cells :: [Position] 
        cells = concatMap surroundingPositions $ filter (\p -> member p visibility && (tallyMines grid p == 0)) $ case move of
            Up        -> [Cartesian i top    | i <- [left..right]]
            Down      -> [Cartesian i bottom | i <- [left..right]]
            Left      -> [Cartesian left i   | i <- [top..bottom]]
            Right     -> [Cartesian right i  | i <- [top..bottom]]
            UpLeft    -> [Cartesian i top    | i <- [left..right]] ++ [Cartesian left i   | i <- [top..bottom]]
            UpRight   -> [Cartesian i top    | i <- [left..right]] ++ [Cartesian right i  | i <- [top..bottom]]
            DownLeft  -> [Cartesian i bottom | i <- [left..right]] ++ [Cartesian left i   | i <- [top..bottom]]
            DownRight -> [Cartesian i bottom | i <- [left..right]] ++ [Cartesian right i  | i <- [top..bottom]]

        -- get the new GameState with the updated cells and panel
        newGameState = foldl getEmptyCells g{panel=newPanel} cells

-- | Implements the AutoOpen option by opening cells surrounding satisfied cells
updateMarker :: Position -> GameUpdate
updateMarker pos g@GameState{visibility=vis}
    | size vis == size (visibility newGameState) = pure newGameState
    | otherwise                                  = updateMarker pos newGameState
        where
            cells :: [Position]
            cells = concatMap surroundingPositions $ filter (\p -> member p vis && isSatisfied g p) (surroundingPositions pos)

            newGameState :: GameState
            newGameState = foldl clickCellPos g cells

-- | Handle the placement of a marker on the grid
placeMarker :: GameUpdate
placeMarker g@GameState{playState=Dead} = pure g
placeMarker g@GameState{markers, visibility, position=pos, options}
    | member pos visibility   = pure g
    | member pos markers      = pure g{markers=delete pos markers}
    | autoOpen options        = updateMarker pos newGameState
    | otherwise               = pure newGameState
    where
        newGameState :: GameState
        newGameState = g{markers=insert pos markers}

-- | Handle a player click on the current cell
clickCell :: GameUpdate
clickCell g@GameState{playState=Dead} = pure g
clickCell g                           = pure $ clickCellPos g (position g)

-- | Handle opening a cell (both user actions on automatic ones)
clickCellPos :: GameState -> Position -> GameState
clickCellPos g@GameState{grid, visibility=vis, markers} pos
    | member pos markers       = g
    | member pos vis           = updatedMarkers
    | getCell grid pos == Mine = g{visibility=insert pos vis, playState=Dead}
    | otherwise                = getEmptyCells g pos
    where
        updatedMarkers :: GameState
        updatedMarkers
          | isSatisfied g pos = foldl clickCellPos g (filter (\p -> not $ member p vis) (surroundingPositions pos))
          | otherwise         = g
