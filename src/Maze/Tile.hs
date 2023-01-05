module Maze.Tile (Tile(..), tilesConnected, rotateTileClockwiseBy) where
import Maze.Connector (Connector(..), sides)
import Maze.Orientation (Orientation(..), rotateClockwiseBy)
import Data.Set (Set, member)
import qualified Data.Set (map)

-- | Represents a piece on the Board, including its shape and orientation
data Tile = Tile Connector Orientation

instance Show Tile where
  show (Tile Bar  North) = "│"
  show (Tile Bar  South) = "│"
  show (Tile Bar  _)     = "─"
  show (Tile L    North) = "└"
  show (Tile L    East)  = "┌"
  show (Tile L    South) = "┐"
  show (Tile L    West)  = "┘"
  show (Tile T    North) = "┬"
  show (Tile T    East)  = "┤"
  show (Tile T    South) = "┴"
  show (Tile T    West)  = "├"
  show (Tile Plus _)     = "┼"

-- | Two tiles are equal if they connect in the same directions
instance Eq Tile where
  t1 == t2 = tileConnections t1 == tileConnections t2

-- | Are tiles `t1` and `t2` connected if `t2` is `orientation` of `t1`?
-- | e.g. Q: are ┼ and ┘ connected if ┘ is East of ┼? A: yes.
tilesConnected :: Tile -> Tile -> Orientation -> Bool
tilesConnected t1 t2 North = tilesConnectedVertical t1 t2
tilesConnected t1 t2 East  = tilesConnectedVertical (rotateTileClockwiseBy t1 West)
                                                    (rotateTileClockwiseBy t2 West)
tilesConnected t1 t2 South = tilesConnectedVertical t2 t1
tilesConnected t1 t2 West  = tilesConnectedVertical (rotateTileClockwiseBy t1 East)
                                                    (rotateTileClockwiseBy t2 East)

-- | Are the two Tiles connected if the first is below the second? 
tilesConnectedVertical :: Tile -> Tile -> Bool
tilesConnectedVertical bottom top = bottomHasDeg0 && topHasDeg180
  where bottomHasDeg0 = member North   $ tileConnections bottom 
        topHasDeg180  = member South $ tileConnections top

-- | Rotate a Tile clockwise by the given Orientation
rotateTileClockwiseBy :: Tile -> Orientation -> Tile
rotateTileClockwiseBy (Tile connector orientation) by = Tile connector $ rotateClockwiseBy orientation by

-- | Produces the set of Orientations that the given Tile connects to
tileConnections :: Tile -> Set Orientation
tileConnections (Tile connector orientation) = 
  Data.Set.map (rotateClockwiseBy orientation) (sides connector) 
