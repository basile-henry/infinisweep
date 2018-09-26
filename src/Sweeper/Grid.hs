{-# LANGUAGE PatternSynonyms #-}
module Sweeper.Grid (Grid, Position(Cartesian), zeroPosition, movePosition, Panel, getCell, surroundingPositions, inBounds, randomGrid) where

import           Data.List     (unfoldr)
import           System.Random (StdGen, split)

-- | Infinite 2D grid of cells
newtype Grid a = Grid [[a]]

-- | Position in the grid
data Position = Position Int Int
  deriving (Eq, Ord)

pattern Cartesian :: Int -> Int -> Position
pattern Cartesian x y = Position x y
{-# COMPLETE Cartesian #-}

zeroPosition :: Position
zeroPosition = Position 0 0

movePosition :: Int -> Int -> Position -> Position
movePosition dx dy (Position x y) = Position (x + dx) (y + dy)

-- | The panel is used as limits for recursing down empty cells (it is supposed
-- to be bigger than the terminal)
type Panel = (Position, Position)

-- Get the index from an infinite list (infinite towards both -∞ and +∞)
-- List indices are like this: [0, 1, -1, 2, -2..]
getIndex :: [a] -> Int -> a
getIndex l i
    | i <= 0    = l!!(-2*i)
    | otherwise = l!!(2*i-1)

-- Get a cell from the 2D infinite grid
-- TODO: rename?
getCell :: Grid a -> Position -> a
getCell (Grid grid) (Position x y) = getIndex (getIndex grid x) y

surroundingPositions :: Position -> [Position]
surroundingPositions (Position x y) = [Position i j | i<-[x-1..x+1], j<-[y-1..y+1], x /= i || y /= j]

inBounds :: Position -> Panel -> Bool
inBounds (Position x y) (Position a b, Position c d) = a <= x && x <= c && b <= y && y <= d

randomGrid :: (StdGen -> (a, StdGen)) -> StdGen -> Grid a
randomGrid f gen = Grid [unfoldr (pure . f) g | g <- unfoldr (pure . split) gen]
