module Orientation (Orientation(..), rotateClockwiseBy, toUnitVector) where
import Coordinate (Coordinate(..))

data Orientation = North
  | East
  | South
  | West
  deriving Eq

instance Ord Orientation where
  compare o1 o2 = compare (orientationToInt o1) (orientationToInt o2)

rotateClockwiseBy :: Orientation -> Orientation -> Orientation
rotateClockwiseBy orientation by = intToOrientation (sum $ map orientationToInt [orientation, by])

toUnitVector :: Orientation -> Coordinate
toUnitVector North = Coordinate 0  (-1)
toUnitVector East  = Coordinate 1    0
toUnitVector South = Coordinate 0    1
toUnitVector West  = Coordinate (-1) 0

orientationToInt :: Orientation -> Integer
orientationToInt North   = 0
orientationToInt East  = 1
orientationToInt South = 2
orientationToInt West = 3

intToOrientation :: Integer -> Orientation
intToOrientation i = [North, East, South, West] !! (fromIntegral i `mod` 4)

