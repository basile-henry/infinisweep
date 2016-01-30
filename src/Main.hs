module Main(main) where

import Data.Set (Set, empty, insert, member)
import Debug.Trace (traceShowId)
import System.Random (StdGen, getStdGen, randomRs, randoms, mkStdGen)
import UI.NCurses

type Grid = [[Cell]]
data Cell = Empty | Mine deriving Eq

type Visibility = Set Position
type Position = (Int, Int)

data GameState = GameState Grid Visibility Position

tallyBombs :: Grid -> Position -> Int
tallyBombs grid (x, y) = sum [1 | i<-[x-1..x+1], j<-[y-1..y+1], i>=0 && j>=0 && grid!!j!!i == Mine]

showGrid :: GameState -> Position -> Position -> String
showGrid gamestate (left, top) (right, bottom) = concat [[showCell gamestate (x,y) | x<-[left..right]] | y<-[top..bottom]]

showCell :: GameState -> Position -> Char
showCell (GameState grid vis _) pos@(x, y)
    | x < 0 || y < 0 = ' '
    | member pos vis = showCell' (grid!!y!!x) (tallyBombs grid pos)
    | otherwise      = '.'--'▢'
    where
        showCell' :: Cell -> Int -> Char
        showCell' Mine  _ = 'X'
        showCell' Empty 0 = ' '
        showCell' Empty t = head $ show t

main :: IO ()
main = do 
    gen <- getStdGen
    runCurses $ do
        setEcho False
        w <- defaultWindow

        let gamestate = GameState ([map (\n -> if n<1 then Mine else Empty) $ randomRs (0, 10 :: Int) (mkStdGen g) | g<-(randoms gen) :: [Int]]) empty (0,0)
        doUpdate w gamestate

doUpdate :: Window -> GameState -> Curses ()
doUpdate w gamestate@(GameState _ _ (x, y)) = do
    updateWindow w $ do
        (sizeY', sizeX') <- windowSize
        let (sizeX, sizeY) = (fromInteger sizeX', fromInteger sizeY')
        let topLeft@(left, top) = (x - (div sizeX 2), y - (div sizeY 2))
        let bottomRight = (left + sizeX - 1, top + sizeY - 3)

        moveCursor 0 0
        drawString $ showGrid gamestate topLeft bottomRight
        moveCursor (sizeY' - 2) 0
        drawString $ replicate sizeX '―'
        moveCursor (sizeY' - 1) 0
        drawString "Some state info"
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
stepGameWorld (EventSpecialKey KeyUpArrow)    (GameState g v (x, y)) = GameState g v (x, max 0 $ y-1)
stepGameWorld (EventSpecialKey KeyDownArrow)  (GameState g v (x, y)) = GameState g v (x, y+1)
stepGameWorld (EventSpecialKey KeyLeftArrow)  (GameState g v (x, y)) = GameState g v (max 0 $ x-1, y)
stepGameWorld (EventSpecialKey KeyRightArrow) (GameState g v (x, y)) = GameState g v (x+1, y)
stepGameWorld (EventCharacter ' ')            (GameState g v pos)    = GameState g (insert pos v) pos
stepGameWorld _ gamestate = gamestate