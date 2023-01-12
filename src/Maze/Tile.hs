module Maze.Tile (Tile(..), tilesConnected, rotateTileClockwiseBy, contents) where
import Maze.Connector (Connector(..), sides)
import Maze.Orientation (Orientation(..), rotateClockwiseBy)
import Data.Set (Set, member)
import qualified Data.Set (map)
import Test.QuickCheck

-- | Represents a piece on the Board, including its shape and orientation
-- | Also holds a value `a'.
data Tile a = Tile Connector Orientation a

instance Show (Tile a) where
  show (Tile Bar  North _) = "│"
  show (Tile Bar  South _) = "│"
  show (Tile Bar  _ _)     = "─"
  show (Tile L    North _) = "└"
  show (Tile L    East  _) = "┌"
  show (Tile L    South _) = "┐"
  show (Tile L    West  _) = "┘"
  show (Tile T    North _) = "┬"
  show (Tile T    East  _) = "┤"
  show (Tile T    South _) = "┴"
  show (Tile T    West  _) = "├"
  show (Tile Plus _ _)     = "┼"

-- | Two tiles are equal if they connect in the same directions
-- | and contain equal `a's.
instance Eq a => Eq (Tile a) where
  t1 == t2 = tileConnections t1 == tileConnections t2 &&
             contents t1 == contents t2

instance Arbitrary a => Arbitrary (Tile a) where
  arbitrary = do
    a <- arbitrary
    connector <- chooseEnum (Bar, Plus)
    orientation <- chooseEnum (North, West)
    return $ Tile connector orientation a

contents :: Tile a -> a
contents (Tile _ _ a) = a

-- | Are tiles `t1` and `t2` connected if `t2` is `orientation` of `t1`?
-- | e.g. Q: are ┼ and ┘ connected if ┘ is East of ┼? A: yes.
tilesConnected :: Tile a -> Tile a -> Orientation -> Bool
tilesConnected t1 t2 North = tilesConnectedVertical t1 t2
tilesConnected t1 t2 East  = tilesConnectedVertical (rotateTileClockwiseBy t1 West)
                                                    (rotateTileClockwiseBy t2 West)
tilesConnected t1 t2 South = tilesConnectedVertical t2 t1
tilesConnected t1 t2 West  = tilesConnectedVertical (rotateTileClockwiseBy t1 East)
                                                    (rotateTileClockwiseBy t2 East)

-- | Are the two Tiles connected if the first is below the second? 
tilesConnectedVertical :: Tile a -> Tile a -> Bool
tilesConnectedVertical bottom top = bottomHasDeg0 && topHasDeg180
  where bottomHasDeg0 = member North   $ tileConnections bottom 
        topHasDeg180  = member South $ tileConnections top

-- | Rotate a Tile clockwise by the given Orientation
rotateTileClockwiseBy :: Tile a -> Orientation -> Tile a
rotateTileClockwiseBy (Tile connector orientation a) by = Tile connector (rotateClockwiseBy orientation by) a

-- | Produces the set of Orientations that the given Tile connects to
tileConnections :: Tile a -> Set Orientation
tileConnections (Tile connector orientation _) = 
  Data.Set.map (rotateClockwiseBy orientation) (sides connector) 
