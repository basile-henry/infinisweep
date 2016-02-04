{-# LANGUAGE LambdaCase #-}

module Main(main) where

import Data.Char (toLower, isAlphaNum)
import Data.List (intercalate)
import Data.Set (Set, empty, insert, delete, member, size, toList)
import Prelude hiding (Either(..))
import qualified Prelude as P
import System.Environment (getArgs)
import System.IO.Error (tryIOError)
import qualified System.IO.Strict as S
import System.Random (StdGen, getStdGen, randomRs, randoms, mkStdGen)
import UI.NCurses (Update,  Window, Curses, Color(..), ColorID, Event(..), Key(..), moveCursor, setColor, drawString, drawLineH, runCurses, setEcho, defaultWindow, newColorID, updateWindow, windowSize, glyphLineH, render, getEvent)

type Grid = [[Cell]]
data Cell = Empty | Mine deriving Eq

type Visibility = Set Position
type Markers    = Set Position
type Position   = (Int, Int)
type Score      = Int
data PlayState  = Alive | Dead deriving Eq
type Panel      = (Position, Position) -- The panel is used as limits for recursing down empty cells (It is supposed to be bigger than the terminal)
data Option     = Adventure | AutoOpen | Density Int deriving (Eq, Ord, Show)
type Options    = Set Option

data Move       = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
data GameState  = GameState
    {
        _grid       :: Grid,
        _visibility :: Visibility,
        _markers    :: Markers,
        _position   :: Position,
        _score      :: Score,
        _highscore  :: Score,
        _playState  :: PlayState,
        _panel      :: Panel,
        _options    :: Options
    }

-- List indices are like this: [0, 1, -1, 2, -2..]
getIndex :: [a] -> Int -> a
getIndex l i
    | i <= 0    = l!!(-2*i)
    | otherwise = l!!(2*i-1)

getCell :: Grid -> Position -> Cell
getCell grid (x, y) = getIndex (getIndex grid x) y

surroundingPositions :: Position -> [Position]
surroundingPositions (x, y) = [(i, j) | i<-[x-1..x+1], j<-[y-1..y+1], x /= i || y /= j]

inBounds :: Position -> Panel -> Bool
inBounds (x, y) ((a, b), (c, d)) = a <= x && x <= c && b <= y && y <= d

tallyMines :: Grid -> Position -> Int
tallyMines grid pos = length $ filter (==Mine) $ map (getCell grid) (surroundingPositions pos)

tallyMarkers :: Markers -> Position -> Int
tallyMarkers markers pos = length $ filter (\m -> member m markers) (surroundingPositions pos)

showGrid :: GameState -> Panel -> Position -> [ColorID] -> Update ()
showGrid gamestate ((left, top), (right, bottom)) (sx, sy) pal = sequence_ [do moveCursor (toInteger $ y - sy) (toInteger $ x - sx); showCell gamestate (x,y) pal | x<-[left..right], y<-[top..bottom]]

showCell :: GameState -> Position -> [ColorID] -> Update ()
showCell GameState{_grid=grid, _visibility=vis, _markers=mar, _playState=playstate} pos pal
    | member pos mar       = do markerColor playstate currentCell; drawString "#";
    | playstate == Dead &&
      currentCell == Mine  = drawMine
    | member pos vis       = showCell' currentCell (tallyMines grid pos)
    | otherwise            = do setColor $ pal!!0; drawString " "
    where
        currentCell :: Cell
        currentCell = getCell grid pos

        showCell' :: Cell -> Int -> Update ()
        showCell' Mine  _ = drawMine
        showCell' Empty 0 = do setColor $ pal!!0; drawString "•";
        showCell' Empty t = do setColor $ pal!!t; drawString $ show t;

        drawMine :: Update ()
        drawMine = do setColor $ pal!!8; drawString "X";

        markerColor :: PlayState -> Cell -> Update ()
        markerColor Dead Empty = setColor $ pal!!2
        markerColor _    _     = setColor $ pal!!8

randomGrid :: StdGen -> Int -> Grid
randomGrid gen den = [map (\n -> if n<den then Mine else Empty) $ randomRs (0, 99 :: Int) (mkStdGen g) | g<-(randoms gen) :: [Int]]

createGameStates :: StdGen -> Options -> Score -> [GameState]
createGameStates gen opts highscore =  map (\g -> GameState 
    {
        _grid       = randomGrid (mkStdGen g) (density $ toList opts),
        _visibility = empty,
        _markers    = empty,
        _position   = (0, 0),
        _score      = 0,
        _highscore  = highscore,
        _playState  = Alive,
        _panel      = ((-150, -50), (150, 50)),
        _options    = opts
    }) ((randoms gen) :: [Int])
    where
        density :: [Option] -> Int
        density []            = 20
        density (Density x:_) = x
        density (_:xs)        = density xs

argsToOptions :: [String] -> Options
argsToOptions []               = empty
argsToOptions ("auto":xs)      = insert AutoOpen $ argsToOptions xs
argsToOptions ("adventure":xs) = insert Adventure $ argsToOptions xs
argsToOptions ("density":x:xs) = insert (Density $ read x) $ argsToOptions xs
argsToOptions (_:xs)           = argsToOptions xs

highscorePath :: Options -> FilePath
highscorePath options = intercalate "_" $ ".highscore" : (map (filter isAlphaNum . show) $ toList options)

readHighscore :: Options -> IO Score
readHighscore options = do
    strOrExc <- tryIOError $ S.readFile $ highscorePath options
    let
        getScore :: [String] -> Score
        getScore []     = 0
        getScore (x:_) = read $ last $ words x

        highscore = case strOrExc of
            P.Left  _        -> 0
            P.Right contents -> getScore $ lines contents

    return highscore

writeHighscore :: Options -> Score -> IO ()
writeHighscore options score = writeFile (highscorePath options) (show score)

main :: IO ()
main = do
    gen <- getStdGen
    args <- getArgs
    let options = argsToOptions $ map (map toLower) args
    highscore <- readHighscore options

    new_highscore <- runCurses $ do
        setEcho False
        w <- defaultWindow
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

        let
            restartLoop :: (GameState -> Curses (Score, Bool)) -> [GameState] -> Curses Score
            restartLoop f (g:ng:gs) = do
                quit <- f g
                case quit of
                    (hs, True)  -> restartLoop f (ng{_highscore=hs}:gs)
                    (hs, False) -> return hs

        restartLoop (doUpdate w palette) (createGameStates gen options highscore)

    writeHighscore options new_highscore


doUpdate :: Window -> [ColorID] -> GameState -> Curses (Score, Bool)
doUpdate w palette g@GameState{_position=(x, y), _score=score, _highscore=highscore, _playState=playstate, _options=opts} = do
    updateWindow w $ do
        (sizeY, sizeX) <- windowSize
        let (sizeX', sizeY') = (fromInteger sizeX, fromInteger sizeY)
        let topLeft@(left, top) = (x - (div sizeX' 2), y - (div sizeY' 2))
        let bottomRight = (left + sizeX' - 1, top + sizeY' - 3)
        let panel = (topLeft, bottomRight)

        moveCursor 0 0
        showGrid g panel (left, top) palette
        moveCursor (sizeY - 2) 0
        setColor $ palette!!2
        drawLineH (Just glyphLineH) sizeX
        moveCursor (sizeY - 1) 0
        setColor $ palette!!0
        drawString $ take (sizeX'-1) $ concat [show o ++ " | " | o <- toList opts] ++ case playstate of
            Alive -> "Score: " ++ show score ++ repeat ' '
            Dead  -> "Game over! Your score is: " ++ show score ++ " | Highscore is: " ++ show highscore ++ repeat ' '
        moveCursor (div sizeY 2) (div sizeX 2)
    render
    inputUpdate w palette g

inputUpdate :: Window -> [ColorID] -> GameState -> Curses (Score, Bool)
inputUpdate w palette g = do
    getEvent w (Just 100) >>= maybe
        (doUpdate w palette g)
        (\case
            EventCharacter 'q' -> return (_highscore g, False)
            EventCharacter 'Q' -> return (_highscore g, False)
            EventCharacter 'r' -> return (_highscore g, True)
            EventCharacter 'R' -> return (_highscore g, True)
            key                -> doUpdate w palette (stepGameWorld key g))

movePosition :: GameState -> Move -> GameState
movePosition g@GameState{_grid=grid, _visibility=vis, _position=(x, y), _panel=((left, top), (right, bottom))} move =
    newGameState{_position = (x+dx, y+dy)}
    where
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

        cells :: [Position]
        cells = concatMap surroundingPositions $ filter (\p -> (tallyMines grid p == 0) && (member p vis)) $ case move of
            Up        -> [(i, top)    | i <- [left..right]]
            Down      -> [(i, bottom) | i <- [left..right]]
            Left      -> [(left, i)   | i <- [top..bottom]]
            Right     -> [(right, i)  | i <- [top..bottom]]
            UpLeft    -> [(i, j)      | i <- [left..right], j <- [top..bottom]]
            UpRight   -> [(i, j)      | i <- [left..right], j <- [top..bottom]]
            DownLeft  -> [(i, j)      | i <- [left..right], j <- [top..bottom]]
            DownRight -> [(i, j)      | i <- [left..right], j <- [top..bottom]]

        newGameState = foldl getEmptyCells g{_panel=newPanel} cells

getEmptyCells :: GameState -> Position -> GameState
getEmptyCells g@GameState{_grid=grid, _panel=panel, _visibility=vis, _markers=markers, _score=score, _highscore=highscore} pos
    | not (inBounds pos panel)
      || member pos vis
      || member pos markers    = g
    | t > 0                    = g{_visibility=newVis, _score=score + t, _highscore=max highscore (score + t)}
    | otherwise                = foldl getEmptyCells g{_visibility=newVis} (surroundingPositions pos)
    where
        t :: Int
        t = tallyMines grid pos

        newVis :: Visibility
        newVis = insert pos vis


isSatisfied :: GameState -> Position -> Bool
isSatisfied GameState{_grid=grid, _markers=markers} p = tallyMines grid p == tallyMarkers markers p

updateMarker :: GameState -> Position -> GameState
updateMarker g@GameState{_visibility=vis} pos
    | size vis == size (_visibility newGameState) = newGameState
    | otherwise                                   = updateMarker newGameState pos    
        where
            cells :: [Position]
            cells = concatMap surroundingPositions $ filter (\p -> (member p vis) && (isSatisfied g p)) (surroundingPositions pos)

            newGameState :: GameState
            newGameState = foldl clickCellPos g cells

placeMarker :: GameState -> GameState
placeMarker g@GameState{_markers=markers, _visibility=vis, _position=pos, _options=opts}
    | member pos vis       = g
    | member pos markers   = g{_markers=(delete pos markers)}
    | member AutoOpen opts = updateMarker newGameState pos 
    | otherwise            = newGameState
    where
        newGameState :: GameState
        newGameState = g{_markers=(insert pos markers)}

clickCell :: GameState -> GameState
clickCell g = clickCellPos g (_position g)

clickCellPos :: GameState -> Position -> GameState
clickCellPos g@GameState{_grid=grid, _visibility=vis, _markers=markers} pos
    | member pos markers       = g
    | member pos vis           = updatedMarkers
    | getCell grid pos == Mine = g{_visibility=(insert pos vis), _playState=Dead}
    | otherwise                = getEmptyCells g pos
    where
        updatedMarkers :: GameState
        updatedMarkers = if (isSatisfied g pos)
            then foldl clickCellPos g (filter (\p -> not $ member p vis) (surroundingPositions pos))
            else g

stepGameWorld :: Event -> GameState -> GameState
stepGameWorld (EventSpecialKey KeyUpArrow)    gamestate                    = movePosition gamestate Up
stepGameWorld (EventCharacter  'w')           gamestate                    = movePosition gamestate Up
stepGameWorld (EventCharacter  'W')           gamestate                    = movePosition gamestate Up
stepGameWorld (EventCharacter  'k')           gamestate                    = movePosition gamestate Up
stepGameWorld (EventCharacter  'K')           gamestate                    = movePosition gamestate Up
stepGameWorld (EventSpecialKey KeyDownArrow)  gamestate                    = movePosition gamestate Down
stepGameWorld (EventCharacter  's')           gamestate                    = movePosition gamestate Down
stepGameWorld (EventCharacter  'S')           gamestate                    = movePosition gamestate Down
stepGameWorld (EventCharacter  'j')           gamestate                    = movePosition gamestate Down
stepGameWorld (EventCharacter  'J')           gamestate                    = movePosition gamestate Down
stepGameWorld (EventSpecialKey KeyLeftArrow)  gamestate                    = movePosition gamestate Left
stepGameWorld (EventCharacter  'a')           gamestate                    = movePosition gamestate Left
stepGameWorld (EventCharacter  'A')           gamestate                    = movePosition gamestate Left
stepGameWorld (EventCharacter  'h')           gamestate                    = movePosition gamestate Left
stepGameWorld (EventCharacter  'H')           gamestate                    = movePosition gamestate Left
stepGameWorld (EventSpecialKey KeyRightArrow) gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'd')           gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'D')           gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'l')           gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'L')           gamestate                    = movePosition gamestate Right
stepGameWorld (EventCharacter  'y')           gamestate                    = movePosition gamestate UpLeft
stepGameWorld (EventCharacter  'Y')           gamestate                    = movePosition gamestate UpLeft
stepGameWorld (EventCharacter  'u')           gamestate                    = movePosition gamestate UpRight
stepGameWorld (EventCharacter  'U')           gamestate                    = movePosition gamestate UpRight
stepGameWorld (EventCharacter  'b')           gamestate                    = movePosition gamestate DownLeft
stepGameWorld (EventCharacter  'B')           gamestate                    = movePosition gamestate DownLeft
stepGameWorld (EventCharacter  'n')           gamestate                    = movePosition gamestate DownRight
stepGameWorld (EventCharacter  'N')           gamestate                    = movePosition gamestate DownRight
stepGameWorld _                               g@GameState{_playState=Dead} = g -- If not playing, player can move around but not "play" (open cells)
stepGameWorld (EventCharacter 'm')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter 'M')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter 'e')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter 'E')            gamestate                    = placeMarker  gamestate
stepGameWorld (EventCharacter ' ')            gamestate                    = clickCell    gamestate
stepGameWorld (EventSpecialKey KeyEnter)      gamestate                    = clickCell    gamestate
stepGameWorld _                               gamestate                    = gamestate

-- animate :: GameState -> GameState
-- animate = animate' 10

-- animate' :: Int -> GameState -> GameState
-- animate' 0 g = g
-- animate' n g@GameState{_playState=Dead, _position=(x, y), _grid=grid, _visibility=vis} = animate' (n-1) revealOneMine
--     where
--         revealOneMine :: GameState
--         revealOneMine = g{_visibility=insert minePos vis}

--         minePos :: Position
--         minePos = head $ filter (\p -> (not $ member p vis) && (getCell grid p == Mine)) $ [(x+i, y+j) | d<-[1..], i<-[-d..d], j<-[-d..d]]
-- animate' _ gamestate                  = gamestate
