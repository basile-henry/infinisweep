{-# LANGUAGE NamedFieldPuns #-}

module Sweeper.Game where

-- base
import           Numeric.Natural
import           Prelude         hiding (Left, Right)

-- random
import           System.Random   (StdGen, randomR, split)

-- infinisweep
import           Sweeper.Grid

data Cell       = Empty Bool | Mark Bool | Visible Int deriving Eq

isMine :: Cell -> Bool
isMine (Empty mine) = mine
isMine (Mark  mine) = mine
isMine _            = False

isVisible :: Cell -> Bool
isVisible Visible{} = True
isVisible _         = False

isMarked :: Cell -> Bool
isMarked Mark{} = True
isMarked _      = False

mark :: Cell -> Cell
mark (Empty mine) = Mark mine
mark cell         = cell

unmark :: Cell -> Cell
unmark (Mark mine) = Empty mine
unmark cell        = cell

-- | The score corresponds to the sum of all the numbers showing in the visible cells
type Score      = Int
data PlayState  = Alive | Dead deriving Eq

data Options = Options
  { autoOpen  :: Bool
  , density   :: Int
  }

prettyShow :: Options -> [String]
prettyShow opts =
  ["Auto Open" | autoOpen opts] ++
  ["Density: " ++ show (density opts)]

data Move       = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight -- Possible ways to move on the grid
data GameState  = GameState
    {
        grid      :: Grid Cell,
        visible   :: Natural,
        score     :: Score,
        position  :: Position,
        highscore :: Score,
        playState :: PlayState,
        panel     :: Panel,
        randomgen :: StdGen,
        options   :: Options
    }

-- | Count the number of mines in the positions around a given position
tallyMines :: Grid Cell -> Position -> Int
tallyMines grid pos = length $ filter isMine $ map (`getCell` grid) (surroundingPositions pos)

-- | Count the number of marked cells in the positions around a given position
tallyMarkers :: Grid Cell -> Position -> Int
tallyMarkers grid pos = length $ filter isMarked $ map (`getCell` grid) (surroundingPositions pos)

-- | Randomly generate a cell given a density
randomCell :: Int -> StdGen -> (Cell, StdGen)
randomCell density gen =
  let (n, g) = randomR (0,99) gen
  in (Empty (n < density), g)

-- | Generate a random initial GameState
createGameState :: StdGen -> Options -> Score -> GameState
createGameState gen opts hs = let (g, g') = split gen in
  GameState
    {
        grid       = randomGrid (randomCell (density opts)) g,
        visible    = 0,
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
getEmptyCells g@GameState{grid, visible, panel, score, highscore} pos
    | not (inBounds pos panel)
      || isMarked (getCell pos grid)
      || isVisible (getCell pos grid) = g
    | t > 0                           = g{grid=newGrid, score=score + t, highscore=max highscore (score + t), visible=visible+1}
    | otherwise                       = foldl getEmptyCells g{grid=newGrid} (surroundingPositions pos)
    where
        t :: Int
        t = tallyMines grid pos

        newGrid :: Grid Cell
        newGrid = setCell pos (Visible t) grid

-- | A Cell (at a given position) is satisfied if the number of Mines around it matches the number of cells
-- Markers could still be missplaced!
isSatisfied :: GameState -> Position -> Bool
isSatisfied GameState{grid} p = tallyMines grid p == tallyMarkers grid p

type GameUpdate = GameState -> Maybe GameState

-- | Change the current position on the grid
makeMove :: Move -> GameUpdate
makeMove move g@GameState{grid, position, panel=(topLeft@(Cartesian left top), bottomRight@(Cartesian right bottom))} =
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
        cells = concatMap surroundingPositions $ filter (\p -> isVisible (getCell p grid) && (tallyMines grid p == 0)) $ case move of
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
updateMarker pos g@GameState{grid,visible=vn}
    | vn == visible newGameState = pure newGameState
    | otherwise                  = updateMarker pos newGameState
        where
            cells :: [Position]
            cells = concatMap surroundingPositions $ filter (\p -> isVisible (getCell p grid) && isSatisfied g p) (surroundingPositions pos)

            newGameState :: GameState
            newGameState = foldl clickCellPos g cells

-- | Handle the placement of a marker on the grid
placeMarker :: GameUpdate
placeMarker g@GameState{playState=Dead} = pure g
placeMarker g@GameState{grid, position=pos, options}
    | isVisible (getCell pos grid) = pure g
    | isMarked (getCell pos grid) = pure g{grid = modifyCell pos unmark grid}
    | autoOpen options        = updateMarker pos newGameState
    | otherwise               = pure newGameState
    where
        newGameState :: GameState
        newGameState = g{grid = modifyCell pos mark grid}

-- | Handle a player click on the current cell
clickCell :: GameUpdate
clickCell g@GameState{playState=Dead} = pure g
clickCell g                           = pure $ clickCellPos g (position g)

-- | Handle opening a cell (both user actions on automatic ones)
clickCellPos :: GameState -> Position -> GameState
clickCellPos g@GameState{grid} pos
    | isMarked (getCell pos grid)  = g
    | isVisible (getCell pos grid) = updatedMarkers
    | isMine (getCell pos grid)    = g{playState=Dead}
    | otherwise                    = getEmptyCells g pos
    where
        updatedMarkers :: GameState
        updatedMarkers
          | isSatisfied g pos = foldl clickCellPos g (filter (\p -> not $ isVisible (getCell p grid)) (surroundingPositions pos))
          | otherwise         = g
