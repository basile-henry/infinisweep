module Main(main) where

import Data.Set (Set, empty, insert, member)
import System.Random (StdGen, getStdGen, randomRs, randoms, mkStdGen)
import UI.NCurses

type Grid = [[Cell]]

data Cell = Empty | Mine deriving Eq

type Visibility = Set Position
type Position = (Int, Int)
type Score = Int
type Playing = Bool
type Palette = [ColorID]

data GameState = GameState Grid Visibility Position Score Playing Palette

surroundingPositions :: Position -> [Position]
surroundingPositions (x, y) = [(i, j) | i<-[x-1..x+1], j<-[y-1..y+1], i>=0 && j>=0 && (x /= i || y /= j)]

tallyMines :: Grid -> Position -> Int
tallyMines grid pos = sum [1 | (x, y)<-(surroundingPositions pos), grid!!y!!x == Mine]

showGrid :: GameState -> Position -> Position -> Position -> Update ()
showGrid gamestate (left, top) (right, bottom) (sx, sy) = sequence_ [do moveCursor (toInteger $ y - sy) (toInteger $ x - sx); showCell gamestate (x,y) | x<-[left..right], y<-[top..bottom]]

showCell :: GameState -> Position -> Update ()
showCell (GameState grid vis _ _ _ pal) pos@(x, y)
    | x < 0 || y < 0 = drawString " "
    | member pos vis = showCell' (grid!!y!!x) (tallyMines grid pos)
    | otherwise      = do setColor $ pal!!0; drawString "•";
    where
        showCell' :: Cell -> Int -> Update ()
        showCell' Mine  _ = do setColor $ pal!!8; drawString "X";
        showCell' Empty 0 = drawString " ";
        showCell' Empty t = do setColor $ pal!!t; drawString $ show t;

getEmptyCells :: Grid -> (Visibility, Score) -> Position -> (Visibility, Score)
getEmptyCells grid (vis, score) pos
    | member pos vis = (vis, score)
    | t > 0          = (new_vis, score + t)
    | otherwise      = foldl (getEmptyCells grid) (new_vis, score) (surroundingPositions pos)
    where
        t :: Int
        t = tallyMines grid pos

        new_vis :: Visibility
        new_vis = insert pos vis

randomGrid :: StdGen -> Grid
randomGrid gen = [map (\n -> if n<1 then Mine else Empty) $ randomRs (0, 7 :: Int) (mkStdGen g) | g<-(randoms gen) :: [Int]]

main :: IO ()
main = do 
    gen <- getStdGen
    runCurses $ do
        setEcho False
        w <- defaultWindow
        palette <- sequence
            [
                newColorID ColorWhite   ColorDefault 1,
                newColorID ColorBlue    ColorDefault 2,
                newColorID ColorYellow  ColorDefault 3,
                newColorID ColorGreen   ColorDefault 4,
                newColorID ColorMagenta ColorDefault 5,
                newColorID ColorCyan    ColorDefault 6,
                newColorID ColorBlack   ColorDefault 7,
                newColorID ColorRed     ColorDefault 8,
                newColorID ColorRed     ColorDefault 9
            ]
        doUpdate w (GameState (randomGrid gen) empty (0,0) 0 True palette)

doUpdate :: Window -> GameState -> Curses ()
doUpdate w gamestate@(GameState _ _ (x, y) score playing palette) = do
    updateWindow w $ do
        (sizeY', sizeX') <- windowSize
        let (sizeX, sizeY) = (fromInteger sizeX', fromInteger sizeY')
        let topLeft@(left, top) = (x - (div sizeX 2), y - (div sizeY 2))
        let bottomRight = (left + sizeX - 1, top + sizeY - 3)

        moveCursor 0 0
        showGrid gamestate topLeft bottomRight (left, top)
        moveCursor (sizeY' - 2) 0
        drawString $ replicate sizeX '═'
        moveCursor (sizeY' - 1) 0
        setColor $ palette!!0
        drawString $ if playing
            then "Score: " ++ show score
            else "Game over! Your score is: " ++ show score
        moveCursor (div sizeY' 2) (div sizeX' 2)
    render
    inputUpdate w gamestate

inputUpdate :: Window -> GameState -> Curses ()
inputUpdate w gamestate = do
        ev <- getEvent w Nothing
        case ev of
            Nothing  -> inputUpdate w gamestate
            Just ev' -> case ev' of
                EventCharacter 'q' -> return()
                EventCharacter 'Q' -> return()
                key                -> doUpdate w (stepGameWorld key gamestate)


stepGameWorld :: Event -> GameState -> GameState
stepGameWorld _                               g@(GameState _ _ _      _ False _) = g
stepGameWorld (EventSpecialKey KeyUpArrow)    (GameState g v (x, y) s p c)       = GameState g v (x, max 0 $ y-1) s p c
stepGameWorld (EventSpecialKey KeyDownArrow)  (GameState g v (x, y) s p c)       = GameState g v (x, y+1) s p c
stepGameWorld (EventSpecialKey KeyLeftArrow)  (GameState g v (x, y) s p c)       = GameState g v (max 0 $ x-1, y) s p c
stepGameWorld (EventSpecialKey KeyRightArrow) (GameState g v (x, y) s p c)       = GameState g v (x+1, y) s p c
stepGameWorld (EventCharacter ' ')            (GameState g vis pos@(x, y) score _ c)
    | g!!y!!x == Mine = GameState g (insert pos vis) pos score False c
    | otherwise       = GameState g new_vis pos new_score True c
    where
        (new_vis, new_score) = getEmptyCells g (vis, score) pos
stepGameWorld _ gamestate = gamestate