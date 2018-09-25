{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main(main) where

import           Data.Hashable       (Hashable (hash))
import           Data.List           (intercalate)
import           Data.Set            (Set, delete, empty, insert, member, size)
import           GHC.Generics        (Generic)
import qualified Options.Applicative as Opt
import           Prelude             hiding (Either (..))
import qualified Prelude             as P
import           System.IO.Error     (tryIOError)
import qualified System.IO.Strict    as S
import           System.Random       (StdGen, getStdGen, mkStdGen, split, randomRs,
                                      randoms)
import           UI.NCurses          (Color (..), ColorID, Curses, Event (..),
                                      Key (..), Update, Window, defaultWindow,
                                      drawLineH, drawString, getEvent,
                                      glyphLineH, moveCursor, newColorID,
                                      render, runCurses, setColor, setEcho,
                                      updateWindow, windowSize)

{-# ANN module ("HLint: ignore Use head") #-}
-- we often use "palette !! x" for some x

type Grid       = [[Cell]]     -- Infinite 2D grid of cells
data Cell       = Empty | Mine deriving Eq

type Visibility = Set Position -- Set of positions (in the grid) that are visible (opened cell)
type Markers    = Set Position -- Set of positions that are marked as containing a mine (wether or not it actually does)
type Position   = (Int, Int)   -- Position in the grid
type Score      = Int          -- The score corresponds to the sum of all the numbers showing in the visible cells
data PlayState  = Alive | Dead deriving Eq
type Panel      = (Position, Position) -- The panel is used as limits for recursing down empty cells (it is supposed to be bigger than the terminal)
data Option     = Adventure    -- Idea unimplemented for now
                | AutoOpen     -- When the player marks a cell, automatically open (make visible) the cells adjacents to the satisfied neighbouring cells
                | Density Int  -- Pourcentage of the density of mines in the grid. Default value is 20%
                deriving (Eq, Ord, Show)

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
        grid       :: Grid,
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

-- Get the index from an infinite list (infinite towards both -∞ and +∞)
-- List indices are like this: [0, 1, -1, 2, -2..]
getIndex :: [a] -> Int -> a
getIndex l i
    | i <= 0    = l!!(-2*i)
    | otherwise = l!!(2*i-1)

-- Get a cell from the 2D infinite grid
getCell :: Grid -> Position -> Cell
getCell grid (x, y) = getIndex (getIndex grid x) y

surroundingPositions :: Position -> [Position]
surroundingPositions (x, y) = [(i, j) | i<-[x-1..x+1], j<-[y-1..y+1], x /= i || y /= j]

inBounds :: Position -> Panel -> Bool
inBounds (x, y) ((a, b), (c, d)) = a <= x && x <= c && b <= y && y <= d

-- Count the number of mines in the positions around a given position
tallyMines :: Grid -> Position -> Int
tallyMines grid pos = length $ filter (==Mine) $ map (getCell grid) (surroundingPositions pos)

-- Count the number of markers in the positions around a given position
tallyMarkers :: Markers -> Position -> Int
tallyMarkers markers pos = length $ filter (`member` markers) (surroundingPositions pos)

showGrid :: GameState -> Panel -> Position -> [ColorID] -> Update ()
showGrid gamestate ((left, top), (right, bottom)) (sx, sy) palette =
    sequence_ [do moveCursor (toInteger $ y - sy) (toInteger $ x - sx); showCell gamestate (x,y) palette | x<-[left..right], y<-[top..bottom]]

showCell :: GameState -> Position -> [ColorID] -> Update ()
showCell GameState{grid, visibility, markers, playState} pos palette
    | member pos markers    = do markerColor playState currentCell; drawString "#";
    | playState == Dead &&
      currentCell == Mine   = drawMine
    | member pos visibility = showCell' currentCell (tallyMines grid pos)
    | otherwise             = do setColor $ palette!!0; drawString " "
    where
        currentCell :: Cell
        currentCell = getCell grid pos

        showCell' :: Cell -> Int -> Update ()
        showCell' Mine  _ = drawMine
        showCell' Empty 0 = do setColor $ palette!!0; drawString "•";
        showCell' Empty t = do setColor $ palette!!t; drawString $ show t;

        drawMine :: Update ()
        drawMine = do setColor $ palette!!8; drawString "X";

        markerColor :: PlayState -> Cell -> Update ()
        markerColor Dead Empty = setColor $ palette!!2
        markerColor _    _     = setColor $ palette!!8

-- Generate an infinite grid
randomGrid :: StdGen -> Int -> Grid
randomGrid gen den = [map (\n -> if n<den then Mine else Empty) $ randomRs (0, 99 :: Int) (mkStdGen g) | g<-randoms gen :: [Int]]

-- Generate a random initial GameState
createGameState :: StdGen -> Options -> Score -> GameState
createGameState gen opts hs = let (g, g') = split gen in
  GameState
    {
        grid       = randomGrid g (density opts),
        visibility = empty,
        markers    = empty,
        position   = (0, 0),
        score      = 0,
        highscore  = hs,
        playState  = Alive,
        panel      = ((-150, -50), (150, 50)),
        randomgen  = g',
        options    = opts
    }

newGame :: GameState -> GameState
newGame GameState{randomgen=gen, options=opts, highscore=hs} = createGameState gen opts hs

-- Highscore file path depends on the options
highscorePath :: Options -> FilePath
highscorePath options = ".highscore_" ++ show (hash options)

readHighscore :: Options -> IO Score
readHighscore options = do
    strOrExc <- tryIOError $ S.readFile $ highscorePath options
    let
        getScore :: [String] -> Score
        getScore []    = 0
        getScore (x:_) = read $ last $ words x

        highscore = case strOrExc of
            P.Left  _        -> 0
            P.Right contents -> getScore $ lines contents

    return highscore

writeHighscore :: Options -> Score -> IO ()
writeHighscore options score = writeFile (highscorePath options) (show score)

main :: IO ()
main = do
    gen  <- getStdGen
    options <- Opt.execParser $ Opt.info (Opt.helper <*> optionsParser) Opt.fullDesc
    highscore <- readHighscore options -- get the saved highscore

    -- Start the UI and the mainloop
    -- get the new highscore
    new_highscore <- runCurses $ do
        setEcho False -- prevent keyboard from writing in the terminal
        w <- defaultWindow
        -- get user defined colors (from the profile)
        palette <- sequence
            [
                newColorID ColorBlue    ColorDefault 1,
                newColorID ColorWhite   ColorDefault 2,
                newColorID ColorYellow  ColorDefault 3,
                newColorID ColorGreen   ColorDefault 4,
                newColorID ColorMagenta ColorDefault 5,
                newColorID ColorCyan    ColorDefault 6,
                newColorID ColorBlack   ColorDefault 7,
                newColorID ColorRed     ColorDefault 8,
                newColorID ColorRed     ColorDefault 9
            ]

        doUpdate w palette (createGameState gen options highscore)

    -- save the new highscore
    writeHighscore options new_highscore

-- Mainloop
-- Update the UI
doUpdate :: Window -> [ColorID] -> GameState -> Curses Score
doUpdate w palette g@GameState{position=(x, y), score, highscore, playState, options} = do
    updateWindow w $ do
        (sizeY, sizeX) <- windowSize
        let (sizeX', sizeY') = (fromInteger sizeX, fromInteger sizeY)
        let topLeft@(left, top) = (x - (sizeX' `div` 2), y - (sizeY' `div` 2))
        let bottomRight = (left + sizeX' - 1, top + sizeY' - 3)
        let panel = (topLeft, bottomRight)

        moveCursor 0 0
        showGrid g panel (left, top) palette
        moveCursor (sizeY - 2) 0
        setColor $ palette!!2
        drawLineH (Just glyphLineH) sizeX
        moveCursor (sizeY - 1) 0
        setColor $ palette!!0
        drawString $ take (sizeX'-1) $
            intercalate " | " (
                prettyShow options ++
                case playState of
                    Alive -> ["Score: " ++ show score]
                    Dead  -> ["Game over! Your score is: " ++ show score, "Highscore is: " ++ show highscore]
                )
            ++ repeat ' '
        moveCursor (div sizeY 2) (div sizeX 2)
    render
    inputUpdate w palette g

-- Take keyboard inputs and update GameState
inputUpdate :: Window -> [ColorID] -> GameState -> Curses Score
inputUpdate w palette g =
    getEvent w (Just 100) >>= maybe
        (doUpdate w palette g)
        (\key -> case stepGameWorld key g of
          Nothing -> pure (highscore g)
          Just g' -> doUpdate w palette g'
          )

-- Recursively open cells that are empty (limited by panel to avoid infinite recursion)
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

-- A Cell (at a given position) is satisfied if the number of Mines around it matches the number of cells
-- Markers could still be missplaced!
isSatisfied :: GameState -> Position -> Bool
isSatisfied GameState{grid, markers} p = tallyMines grid p == tallyMarkers markers p

type GameUpdate = GameState -> Maybe GameState

-- Change the current position on the grid
movePosition :: Move -> GameUpdate
movePosition move g@GameState{grid, visibility, position=(x, y), panel=((left, top), (right, bottom))} =
    pure newGameState{position = (x+dx, y+dy)}
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
        newPanel = ((left+dx, top+dy), (right+dx, bottom+dy))

        -- cells on the edge of the panel that need to be updated because the panel is moving
        -- this update is necessary since the cells opened recursively stopped at the edge of the panel
        cells :: [Position]
        cells = concatMap surroundingPositions $ filter (\p -> member p visibility && (tallyMines grid p == 0)) $ case move of
            Up        -> [(i, top)    | i <- [left..right]]
            Down      -> [(i, bottom) | i <- [left..right]]
            Left      -> [(left, i)   | i <- [top..bottom]]
            Right     -> [(right, i)  | i <- [top..bottom]]
            UpLeft    -> [(i, top)    | i <- [left..right]] ++ [(left, i)   | i <- [top..bottom]]
            UpRight   -> [(i, top)    | i <- [left..right]] ++ [(right, i)  | i <- [top..bottom]]
            DownLeft  -> [(i, bottom) | i <- [left..right]] ++ [(left, i)   | i <- [top..bottom]]
            DownRight -> [(i, bottom) | i <- [left..right]] ++ [(right, i)  | i <- [top..bottom]]

        -- get the new GameState with the updated cells and panel
        newGameState = foldl getEmptyCells g{panel=newPanel} cells

-- Implements the AutoOpen option by opening cells surrounding satisfied cells
updateMarker :: Position -> GameUpdate
updateMarker pos g@GameState{visibility=vis}
    | size vis == size (visibility newGameState) = pure newGameState
    | otherwise                                  = updateMarker pos newGameState
        where
            cells :: [Position]
            cells = concatMap surroundingPositions $ filter (\p -> member p vis && isSatisfied g p) (surroundingPositions pos)

            newGameState :: GameState
            newGameState = foldl clickCellPos g cells

-- Handle the placement of a marker on the grid
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

-- Handle a player click on the current cell
clickCell :: GameUpdate
clickCell g@GameState{playState=Dead} = pure g
clickCell g                           = pure $ clickCellPos g (position g)

-- Handle opening a cell (both user actions on automatic ones)
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

-- Handle keyboard inputs on the current GameState and update the GameState accordingly
stepGameWorld :: Event -> GameUpdate
stepGameWorld event
    | event `elem` quitEvents          = \_ -> Nothing
    | event `elem` restartEvents       = pure . newGame 
    | event `elem` moveUpEvents        = movePosition Up
    | event `elem` moveDownEvents      = movePosition Down
    | event `elem` moveLeftEvents      = movePosition Left
    | event `elem` moveRightEvents     = movePosition Right
    | event `elem` moveUpLeftEvents    = movePosition UpLeft
    | event `elem` moveUpRightEvents   = movePosition UpRight
    | event `elem` moveDownLeftEvents  = movePosition DownLeft
    | event `elem` moveDownRightEvents = movePosition DownRight
    | event `elem` placeMarkerEvents   = placeMarker
    | event `elem` clickCellEvents     = clickCell
    | otherwise = pure
  where
    quitEvents          =                                 map EventCharacter "qQ"
    restartEvents       =                                 map EventCharacter "rR"
    moveUpEvents        = EventSpecialKey KeyUpArrow    : map EventCharacter "wWkK8"
    moveDownEvents      = EventSpecialKey KeyDownArrow  : map EventCharacter "sSjJ2"
    moveLeftEvents      = EventSpecialKey KeyLeftArrow  : map EventCharacter "aAhH4"
    moveRightEvents     = EventSpecialKey KeyRightArrow : map EventCharacter "dDlL6"
    moveUpLeftEvents    =                                 map EventCharacter "yY7"
    moveUpRightEvents   =                                 map EventCharacter "uU9"
    moveDownLeftEvents  =                                 map EventCharacter "bB1"
    moveDownRightEvents =                                 map EventCharacter "nN3"
    placeMarkerEvents   =                                 map EventCharacter "mMeE5"
    clickCellEvents     = EventSpecialKey KeyEnter      : map EventCharacter " 0"
