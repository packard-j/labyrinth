module Orientation (Orientation(..), rotateClockwiseBy, toUnitVector) where
import Coordinate (Coordinate(..))

-- | Represents a direction along the Board, where North is up (towards y == 0)
data Orientation = North
  | East
  | South
  | West
  deriving Eq

instance Ord Orientation where
  compare o1 o2 = compare (fromEnum o1) (fromEnum o2)

instance Show Orientation where
  show North = "↑"
  show East  = "→"
  show South = "↓"
  show West  = "←"

instance Enum Orientation where
  -- | The number of 90-degree clockwise rotations from North to reach the given Orientation
  fromEnum North = 0
  fromEnum East  = 1
  fromEnum South = 2
  fromEnum West  = 3
  -- | The Orientation reached after rotating clockwise 90 degrees `i` times from North
  toEnum i = [North, East, South, West] !! (i `mod` 4)

-- | Rotates an orientation clockwise by the angle that the second orientation makes with North
rotateClockwiseBy :: Orientation -> Orientation -> Orientation
rotateClockwiseBy orientation by = toEnum (sum $ map fromEnum [orientation, by])

-- | Procuces the unit vector that points in the direction of the given Orientation
toUnitVector :: Orientation -> Coordinate
toUnitVector North = Coordinate 0  (-1)
toUnitVector East  = Coordinate 1    0
toUnitVector South = Coordinate 0    1
toUnitVector West  = Coordinate (-1) 0
