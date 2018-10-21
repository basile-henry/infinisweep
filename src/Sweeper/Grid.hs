{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Sweeper.Grid (Grid, Position(Cartesian), zeroPosition, movePosition, Panel, getCell, surroundingPositions, inBounds, randomGrid, setCell, modifyCell) where

-- random
import           System.Random                (StdGen, split)

-- infinisweep
import           Sweeper.Grid.BalancedTernary

-- | Infinite 2D grid of cells
newtype Grid a = Grid (Stream (Stream a))

-- | Position in the grid
data Position = Position Index Index
  deriving (Eq, Ord)

pattern Cartesian :: Integer -> Integer -> Position
pattern Cartesian x y <- Position (fromIndex -> x) (fromIndex -> y)
  where
    Cartesian x y = Position (toIndex x) (toIndex y)
{-# COMPLETE Cartesian #-}

zeroPosition :: Position
zeroPosition = Position mempty mempty

movePosition :: Integer -> Integer -> Position -> Position
movePosition dx dy (Position x y) = Position (x <> toIndex dx) (y <> toIndex dy)

-- | The panel is used as limits for recursing down empty cells (it is supposed
-- to be bigger than the terminal)
type Panel = (Position, Position)

-- Get a cell from the 2D infinite grid
-- TODO: rename?
getCell :: Position -> Grid a  -> a
getCell (Position x y) (Grid grid) = index y (index x grid)

setCell :: Position -> a -> Grid a -> Grid a
setCell (Position x y) a (Grid grid) = Grid $ update x (update y (const a)) grid

modifyCell :: Position -> (a -> a) -> Grid a -> Grid a
modifyCell (Position x y) f (Grid grid) = Grid $ update x (update y f) grid

surroundingPositions :: Position -> [Position]
surroundingPositions (Position x y) = [Position i j | i<-[pred x..succ x], j<-[pred y..succ y], x /= i || y /= j]

inBounds :: Position -> Panel -> Bool
inBounds (Cartesian x y) (Cartesian a b, Cartesian c d) = a <= x && x <= c && b <= y && y <= d

randomGrid :: (StdGen -> (a, StdGen)) -> StdGen -> Grid a
randomGrid f gen =
  Grid $ randomStream (\g -> let (g0, g1) = split g in (randomStream f g0, g1)) gen
