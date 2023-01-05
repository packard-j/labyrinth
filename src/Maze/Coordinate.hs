module Maze.Coordinate (Coordinate(..), add) where

-- | Represents a position on a Board as a (column, row) index
-- | where (0, 0) is the top-left and (width-1, height-1) is the bottom right.
data Coordinate = Coordinate Integer Integer deriving Eq

instance Show Coordinate where
  show (Coordinate x y) = show (x,y)

instance Ord Coordinate where
  compare (Coordinate x1 y1) (Coordinate x2 y2)
    | x1 < x2              = LT
    | x1 == x2 && y1 < y2  = LT
    | x1 == x2 && y1 == y2 = EQ
    | otherwise            = GT

-- | Sums the values of two coordinates
add :: Coordinate -> Coordinate -> Coordinate
add (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)
