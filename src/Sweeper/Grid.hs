module Sweeper.Grid (Grid, Position, Panel, getCell, surroundingPositions, inBounds) where

-- | Infinite 2D grid of cells
type Grid a = [[a]]

-- | Position in the grid
type Position = (Int, Int)

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
getCell grid (x, y) = getIndex (getIndex grid x) y

surroundingPositions :: Position -> [Position]
surroundingPositions (x, y) = [(i, j) | i<-[x-1..x+1], j<-[y-1..y+1], x /= i || y /= j]

inBounds :: Position -> Panel -> Bool
inBounds (x, y) ((a, b), (c, d)) = a <= x && x <= c && b <= y && y <= d
