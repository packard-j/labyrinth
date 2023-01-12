module Maze.Connector (Connector(..), sides) where
import Maze.Orientation (Orientation(..))
import Data.Set (Set, fromList)
import Test.QuickCheck

-- | Represents the "shape" of a Tile, which determines the sides it makes connections to
data Connector = Bar 
  | L 
  | T 
  | Plus
  deriving (Eq, Enum)

instance Arbitrary Connector where
  arbitrary = chooseEnum (Bar, Plus)

-- | Produces the set of Orientations that the Connector can connect to
sides :: Connector -> Set Orientation
sides Bar  = fromList [North, South             ]
sides L    = fromList [North, East              ]
sides T    = fromList [East,  South, West       ]
sides Plus = fromList [North, East,  South, West]

