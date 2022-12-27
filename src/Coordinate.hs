module Coordinate (Coordinate(..), add) where

data Coordinate = Coordinate Integer Integer deriving Eq

instance Show Coordinate where
  show (Coordinate x y) = show (x,y)

instance Ord Coordinate where
  compare (Coordinate x1 y1) (Coordinate x2 y2)
    | x1 < x2              = LT
    | x1 == x2 && y1 < y2  = LT
    | x1 == x2 && y1 == y2 = EQ
    | otherwise            = GT

add :: Coordinate -> Coordinate -> Coordinate
add (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)
