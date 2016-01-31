module Main(main) where

import Data.Set (Set, empty, insert, delete, member, notMember)
import Prelude hiding (Either(..))
import System.Random (StdGen, getStdGen, randomRs, randoms, mkStdGen)
import UI.NCurses (Update,  Window, Curses, Color(..), ColorID, Event(..), Key(..), moveCursor, setColor, drawString, drawLineH, runCurses, setEcho, defaultWindow, newColorID, updateWindow, windowSize, glyphLineH, render, getEvent)

type Grid = [[Cell]]

data Cell = Empty | Mine deriving Eq

type Visibility = Set Position
type Markers    = Set Position
type Position   = (Int, Int)
data Move       = Up | Down | Left | Right
type Panel      = (Position, Position)
type Score      = Int
data PlayState  = Alive | Dead deriving Eq
data GameState  = GameState
    {
        _grid       :: Grid,
        _visibility :: Visibility,
        _markers    :: Markers,
        _position   :: Position,
        _score      :: Score,
        _playState  :: PlayState,
        _panel      :: Panel
    }

-- List indeces are like this: [0, 1, -1, 2, -2..]
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
    | member pos mar             = do setColor $ pal!!8; drawString "!";
    | member pos vis             = showCell' currentCell (tallyMines grid pos)
    | playstate == Dead &&
      currentCell == Mine        = drawMine
    | otherwise                  = drawString " "
    where
        currentCell :: Cell
        currentCell = getCell grid pos

        showCell' :: Cell -> Int -> Update ()
        showCell' Mine  _ = drawMine
        showCell' Empty 0 = do setColor $ pal!!0; drawString "•";
        showCell' Empty t = do setColor $ pal!!t; drawString $ show t;

        drawMine :: Update ()
        drawMine = do setColor $ pal!!8; drawString "X";

randomGrid :: StdGen -> Grid
randomGrid gen = [map (\n -> if n<1 then Mine else Empty) $ randomRs (0, 4 :: Int) (mkStdGen g) | g<-(randoms gen) :: [Int]]

createGameStates :: StdGen -> [GameState]
createGameStates gen =  map (\g -> GameState 
    {
        _grid       = randomGrid (mkStdGen g),
        _visibility = empty,
        _markers    = empty,
        _position   = (0, 0),
        _score      = 0,
        _playState  = Alive,
        _panel      = ((-150, -50), (150, 50))
    }) ((randoms gen) :: [Int])

main :: IO ()
main = do 
    gen <- getStdGen
    runCurses $ do
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
            restartLoop :: (a -> Curses Bool) -> [a] -> Curses ()
            restartLoop f (g:gs) = do
                quit <- f g
                case quit of
                    True  -> restartLoop f gs
                    False -> return ()

        restartLoop (doUpdate w palette) (createGameStates gen)

doUpdate :: Window -> [ColorID] -> GameState -> Curses Bool
doUpdate w palette gamestate@(GameState _ _ _ (x, y) score playstate _) = do
    updateWindow w $ do
        (sizeY, sizeX) <- windowSize
        let (sizeX', sizeY') = (fromInteger sizeX, fromInteger sizeY)
        let topLeft@(left, top) = (x - (div sizeX' 2), y - (div sizeY' 2))
        let bottomRight = (left + sizeX' - 1, top + sizeY' - 3)
        let panel = (topLeft, bottomRight)

        moveCursor 0 0
        showGrid gamestate panel (left, top) palette
        moveCursor (sizeY - 2) 0
        setColor $ palette!!2
        drawLineH (Just glyphLineH) sizeX
        moveCursor (sizeY - 1) 0
        setColor $ palette!!0
        drawString $ take (sizeX'-1) $ case playstate of
            Alive -> "Score: " ++ show score ++ repeat ' '
            Dead  -> "Game over! Your score is: " ++ show score ++ repeat ' '
        moveCursor (div sizeY 2) (div sizeX 2)
        -- setTouched True
    render
    inputUpdate w palette gamestate

inputUpdate :: Window -> [ColorID] -> GameState -> Curses Bool
inputUpdate w palette gamestate = do
        ev  <- getEvent w (Just 100)
        case ev of
            Nothing  -> doUpdate w palette (gamestate)
            Just ev' -> case ev' of
                EventCharacter 'q' -> return False
                EventCharacter 'Q' -> return False
                EventCharacter 'r' -> return True
                EventCharacter 'R' -> return True
                key                -> doUpdate w palette (stepGameWorld key gamestate)

movePosition :: GameState -> Move -> GameState
movePosition g@(GameState grid vis _ (x, y) _ _ ((left, top), (right, bottom))) move =
    newGameState{_position = (x+dx, y+dy)}
    where
        (dx, dy) = case move of
            Up    -> ( 0, -1)
            Down  -> ( 0,  1)
            Left  -> (-1,  0)
            Right -> ( 1,  0)

        newPanel :: Panel
        newPanel = ((left+dx, top+dy), (right+dx, bottom+dy))

        cells :: [Position]
        cells = concatMap surroundingPositions $ filter (\p -> (tallyMines grid p == 0) && (member p vis)) $ case move of
            Up    -> [(i, top)    | i <- [left..right]]
            Down  -> [(i, bottom) | i <- [left..right]]
            Left  -> [(left, i)   | i <- [top..bottom]]
            Right -> [(right, i)  | i <- [top..bottom]]

        newGameState = foldl getEmptyCells g{_panel=newPanel} cells

getEmptyCells :: GameState -> Position -> GameState
getEmptyCells g@GameState{_grid=grid, _panel=panel, _visibility=vis, _markers=markers, _score=score} pos
    | member pos vis ||
      not (inBounds pos panel) = g
    | member pos markers       = updateMarker g pos
    | t > 0                    = g{_visibility=newVis, _score=score + t}
    | otherwise                = foldl getEmptyCells g{_visibility=newVis} (surroundingPositions pos)
    where
        t :: Int
        t = tallyMines grid pos

        newVis :: Visibility
        newVis = insert pos vis

updateMarker :: GameState -> Position -> GameState
updateMarker g@GameState{_grid=grid, _visibility=vis, _markers=markers} pos
    | notMember pos markers = error "Update marker at a non marker position."
    | otherwise             = newGameState
        where
            newMarkers :: Markers
            newMarkers = insert pos markers

            isSatisfied :: Position -> Bool
            isSatisfied p = (member p vis) && (tallyMines grid p == tallyMarkers newMarkers p)

            cells :: [Position]
            cells = concatMap surroundingPositions $ filter isSatisfied (surroundingPositions pos)

            newGameState = foldl clickCellPos g{_markers=newMarkers} cells

placeMarker :: GameState -> GameState
placeMarker g@GameState{_markers=markers, _visibility=vis, _position=pos}
    | member pos vis     = g
    | member pos markers = g{_markers=(delete pos markers)}
    | otherwise          = updateMarker (g{_markers=insert pos markers}) pos

clickCell :: GameState -> GameState
clickCell g = clickCellPos g (_position g)

clickCellPos :: GameState -> Position -> GameState
clickCellPos g@GameState{_grid=grid, _visibility=vis, _markers=markers} pos
    | member pos markers       = g
    | getCell grid pos == Mine = g{_visibility=(insert pos vis), _playState=Dead}
    | otherwise                = getEmptyCells g pos

stepGameWorld :: Event -> GameState -> GameState
stepGameWorld (EventSpecialKey KeyUpArrow)    gamestate                    = movePosition gamestate Up
stepGameWorld (EventSpecialKey KeyDownArrow)  gamestate                    = movePosition gamestate Down
stepGameWorld (EventSpecialKey KeyLeftArrow)  gamestate                    = movePosition gamestate Left
stepGameWorld (EventSpecialKey KeyRightArrow) gamestate                    = movePosition gamestate Right
stepGameWorld _                               g@GameState{_playState=Dead} = g -- If not playing, player can move around but not "play" (open cells)
stepGameWorld (EventCharacter 'm')            gamestate                    = placeMarker gamestate
stepGameWorld (EventCharacter 'M')            gamestate                    = placeMarker gamestate
stepGameWorld (EventCharacter ' ')            gamestate                    = clickCell gamestate
stepGameWorld (EventSpecialKey KeyEnter)      gamestate                    = clickCell gamestate
stepGameWorld _ gamestate = gamestate

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
