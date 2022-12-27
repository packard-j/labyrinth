module Connector (Connector(..), sides) where
import Orientation (Orientation(..))
import Data.Set (Set, fromList)

data Connector = Bar 
  | L 
  | T 
  | Plus
  deriving Eq

sides :: Connector -> Set Orientation
sides Bar  = fromList [North, South             ]
sides L    = fromList [North, East              ]
sides T    = fromList [East,  South, West       ]
sides Plus = fromList [North, East,  South, West]

