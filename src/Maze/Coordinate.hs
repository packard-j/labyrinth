{-# LANGUAGE DeriveGeneric #-}
module Maze.Coordinate (Coordinate(..), add) where
import GHC.Generics
import Test.QuickCheck

-- | Represents a position on a Board as a (column, row) index
-- | where (0, 0) is the top-left and (width-1, height-1) is the bottom right.
data Coordinate = Coordinate Integer Integer deriving (Eq, Generic)

instance Show Coordinate where
  show (Coordinate x y) = show (x,y)

instance Ord Coordinate where
  compare (Coordinate x1 y1) (Coordinate x2 y2)
    | x1 < x2              = LT
    | x1 == x2 && y1 < y2  = LT
    | x1 == x2 && y1 == y2 = EQ
    | otherwise            = GT

instance Arbitrary Coordinate where
  arbitrary = do
    x <- chooseInteger (lower, upper)
    y <- chooseInteger (lower, upper)
    return $ Coordinate x y
      where lower = -100
            upper = 100

instance CoArbitrary Coordinate where
  coarbitrary = genericCoarbitrary

-- | Sums the values of two coordinates
add :: Coordinate -> Coordinate -> Coordinate
add (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)
