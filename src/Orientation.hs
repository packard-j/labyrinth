module Orientation (Orientation(..), rotateClockwiseBy, toUnitVector) where
import Coordinate (Coordinate(..))

-- | Represents a direction along the Board, where North is up (towards y == 0)
data Orientation = North
  | East
  | South
  | West
  deriving Eq

instance Ord Orientation where
  compare o1 o2 = compare (orientationToInt o1) (orientationToInt o2)

-- | Rotates an orientation clockwise by the angle that the second orientation makes with North
rotateClockwiseBy :: Orientation -> Orientation -> Orientation
rotateClockwiseBy orientation by = intToOrientation (sum $ map orientationToInt [orientation, by])

-- | Procuces the unit vector that points in the direction of the given Orientation
toUnitVector :: Orientation -> Coordinate
toUnitVector North = Coordinate 0  (-1)
toUnitVector East  = Coordinate 1    0
toUnitVector South = Coordinate 0    1
toUnitVector West  = Coordinate (-1) 0

-- | The number of 90-degree clockwise rotations from North to reach the given Orientation
orientationToInt :: Orientation -> Integer
orientationToInt North = 0
orientationToInt East  = 1
orientationToInt South = 2
orientationToInt West  = 3

-- | The Orientation reached after rotating clockwise 90 degrees `i` times from North
intToOrientation :: Integer -> Orientation
intToOrientation i = [North, East, South, West] !! (fromIntegral i `mod` 4)

