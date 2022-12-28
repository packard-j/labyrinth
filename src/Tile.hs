module Tile (Tile(..), tilesConnected) where
import Connector (Connector(..), sides)
import Orientation (Orientation(..), rotateClockwiseBy)
import Data.Set (Set, member)
import qualified Data.Set (map)

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

instance Eq Tile where
  t1 == t2 = tileConnections t1 == tileConnections t2

tilesConnected :: Tile -> Tile -> Orientation -> Bool
tilesConnected t1 t2 North = tilesConnectedVertical t1 t2
tilesConnected t1 t2 East  = tilesConnectedVertical (rotateTileClockwiseBy t1 West)
                                                    (rotateTileClockwiseBy t2 West)
tilesConnected t1 t2 South = tilesConnectedVertical t2 t1
tilesConnected t1 t2 West  = tilesConnectedVertical (rotateTileClockwiseBy t1 East)
                                                    (rotateTileClockwiseBy t2 East)
rotateTileClockwiseBy :: Tile -> Orientation -> Tile
rotateTileClockwiseBy (Tile connector orientation) by = Tile connector $ rotateClockwiseBy orientation by

tilesConnectedVertical :: Tile -> Tile -> Bool
tilesConnectedVertical bottom top = bottomHasDeg0 && topHasDeg180
  where bottomHasDeg0 = member North   $ tileConnections bottom 
        topHasDeg180  = member South $ tileConnections top

tileConnections :: Tile -> Set Orientation
tileConnections (Tile connector orientation) = 
  Data.Set.map (rotateClockwiseBy orientation) (sides connector) 

