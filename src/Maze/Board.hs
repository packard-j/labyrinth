{-# LANGUAGE TupleSections #-}
module Maze.Board
  (Board,
   BoardResult, BoardError(..),
   Axis(..),
   newBoard,
   slide, shiftAlong,
   reachableTiles, pathExists,
   tileAtSafe, tiles,
   isOnBoard, isOnFixedTile) where
import Maze.Tile (Tile(..), tilesConnected)
import Maze.Coordinate (Coordinate(..), add)
import Maze.Orientation (Orientation(..), toUnitVector, rotateClockwiseBy)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.Set (Set, fromList, member)
import Control.Monad.Except

-- | Represents the game board of labyrinth, which consists of Tiles that
-- | can be slid along rows or columns.
-- | Only certain rows and columns can be slid, as designated by the `movable` field.
-- | A Board of type `a' has `a's on every Tile.
data Board a = Board 
  { tiles  :: [[Tile a]],
    -- | The width of the board, in number of columns
    width  :: Integer,
    -- | The height of the board, in number of columns
    height :: Integer,
    -- | The set of axes that can be shifted, and their 0-indexed position on the board (from top-left)
    movable :: Set AxisIndex } deriving Eq

instance Show (Board a) where
   show board = 
     show (width board) ++ "x" ++ show (height board) ++ " board" ++ "\n" ++
     unlines (map (concatMap show) (tiles board))

type BoardResult = Except BoardError
data BoardError = OutOfBounds (Either AxisIndex Coordinate)
                  | Immovable AxisIndex deriving (Show, Eq)

-- | Represents a row or column along a Board
data Axis = Row | Column deriving (Eq, Ord, Show)
type AxisIndex = (Axis, Integer)

-- | Create a new Board consisting of tiles using the `createTile` mapping of coordinates to tiles,
-- | with the size specified by the given width and height
newBoard :: (Coordinate -> Tile a) -> Integer -> Integer -> Board a
newBoard createTile w h =
  Board 
    (map boardRow [0..h-1])
    w h
    (fromList movableAxes)
  where boardRow row =
          map (\col -> createTile $ Coordinate col row) [0..w-1]
        movableAxes = movableRows ++ movableCols
        movableRows = map (Row,)    (filter even [0..h-1])
        movableCols = map (Column,) (filter even [0..w-1])

-- | Slide an axis (row or column) of a board by inserting a tile, producing a shifted board and a new
-- | spare tile that slid out from the other side.
-- | The axis is specified by the Orientation of the slide and the 0-indexed position of the axis.
-- | For example:
-- |  Sliding North with index 0 slides the leftmost column up, inserting the new tile at the bottom
-- |  and producing a new spare tile that came from the top of that column.
-- | If the axis is not movable or the index is out of bounds, Nothing is returned.
slide :: Board a -> Tile a -> Orientation -> Integer -> BoardResult (Board a, Tile a)
slide board insertedTile dir index
  | index < 0 || index >= bound axis board = throwError $ OutOfBounds $ Left (axis, index)
  | not $ member (axis, index) (movable board) = throwError $ Immovable (axis, index)
  | otherwise = return (shiftedBoard, newSpare board dir index) 
  where
    axis = toSlideAxis dir
    shiftedBoard = mapBoard board slideTiles
    slideTiles coord
      | axisComponent coord /= index = tileAt board coord
      | coord == insertPosition board dir index = insertedTile
      | otherwise = tileAt board $ add coord offset
    axisComponent (Coordinate x y) = if axis == Row then y else x
    offset = toUnitVector $ rotateClockwiseBy dir South
    bound Row    = width
    bound Column = height

-- | Produces the coordinate at which a spare tile will be inserted for
-- | the given shift direction and (valid) axis index
insertPosition :: Board a -> Orientation -> Integer -> Coordinate
insertPosition board North index = Coordinate index $ height board - 1
insertPosition     _ South index = Coordinate index 0
insertPosition     _ East  index = Coordinate 0 index
insertPosition board West  index = Coordinate (width board - 1) index

-- | Produces the tile that becomes the spare after a shift in the given direction
-- | along the axis specified by the (valid) axis index
newSpare :: Board a -> Orientation -> Integer -> Tile a
newSpare board dir index = tileAt board $ sparePosition dir where
  sparePosition North = Coordinate index 0
  sparePosition South = Coordinate index $ height board - 1
  sparePosition West  = Coordinate 0 index
  sparePosition East  = Coordinate (width board - 1) index

-- | Produces the axis along which a slide towards the given Orientation will occur
toSlideAxis :: Orientation -> Axis
toSlideAxis North = Column
toSlideAxis South = Column
toSlideAxis     _ = Row

-- | Produces new equal sized board from the given board with tiles specified by `f`
mapBoard :: Board a -> (Coordinate -> Tile a) -> Board a
mapBoard board f = newBoard f (width board) (height board)

-- | Access the Tile on the Board at the given Coordinate
-- | If the coordinate is outside the bounds of the Board, Nothing is returned
tileAtSafe :: Board a -> Coordinate -> BoardResult (Tile a)
tileAtSafe board coordinate
  | isOnBoard board coordinate = return $ tileAt board coordinate
  | otherwise = throwError $ OutOfBounds $ Right coordinate

-- | What are the coordinates of the tiles that can be reached from the tile at the given coordinate?
-- | This corresponds to the given tile, all the tiles it directly connects to, and all the 
-- | tiles reachable from those.
reachableTiles :: Board a -> Coordinate -> BoardResult (Set Coordinate)
reachableTiles board coordinate = 
  let (graph, coordFromVertex, vertexFromCoord) = toGraph board in 
    case do
     vertex <- vertexFromCoord coordinate
     return $ fromList (map coordFromVertex $ reachable graph vertex) of
    Just coords -> return coords
    Nothing     -> throwError $ OutOfBounds $ Right coordinate

-- | Is the tile at the given coordinate reachable from the other coordinate?
pathExists :: Board a -> Coordinate -> Coordinate -> BoardResult Bool
pathExists board from to = member to <$> reachableTiles board from

-- | Shifts a coordinate along with a slide action as specified by the orientation and index of the axis.
shiftAlong :: Board a -> Orientation -> Integer -> Coordinate -> Coordinate
shiftAlong board dir index from = if affected then to else from where
  affected = case (toSlideAxis dir, from) of
               (Row,    Coordinate _ y) -> y == index
               (Column, Coordinate x _) -> x == index
  to = slideWrap board dir from

-- | Slide a coordinate in the specified direction, looping around to the other side if it reaches
-- | the end.
slideWrap :: Board a -> Orientation -> Coordinate -> Coordinate
slideWrap board dir coord = wrap $ add coord (toUnitVector dir) where
  wrap (Coordinate x y) = Coordinate (x `mod` width board) (y `mod` height board)

-- | Is the given coordinate within the bounds of the board?
isOnBoard :: Board a -> Coordinate -> Bool
isOnBoard board (Coordinate x y)
  | x < 0 || x >= width board  = False
  | y < 0 || y >= height board = False
  | otherwise                  = True

-- | Does the given coordinate correspond to a tile that cannot be moved?
isOnFixedTile :: Board a -> Coordinate -> Bool
isOnFixedTile board (Coordinate x y) = 
  isOnBoard board (Coordinate x y) && onImmovableRow && onImmovableCol where
  onImmovableRow = not $ member (Column, x) (movable board)
  onImmovableCol = not $ member (Row, y)    (movable board)

-- | Access the tile at the given coordinate, without bounds checking.
tileAt :: Board a -> Coordinate -> Tile a
tileAt board (Coordinate x y) = tiles board !! fromIntegral y !! fromIntegral x

-- | Convert the board to a Graph, and functions that map to and from Graph vertices
toGraph :: Board a -> (Graph, Vertex -> Coordinate, Coordinate -> Maybe Vertex)
toGraph board = (graph, coordFromVertex, vertexFromCoord)
  where (graph, nodeFromVertex, vertexFromCoord) = graphFromEdges $ toAdjacencyList board
        coordFromVertex v = second (nodeFromVertex v)
        second (_, x, _) = x

-- | Convert the board to an adjacency list of tiles, their coordinates, and the coordinates
-- | of the tiles they directly connect to.
toAdjacencyList :: Board a -> [(Tile a, Coordinate, [Coordinate])]
toAdjacencyList board = map toNode $ tileList board
  where toNode (coord, tile) = (tile, coord, map fst (connectedAdjacentTiles board coord))

-- | Produces a list of coordinates on a board, paired with the tiles at those coordinates.
tileList :: Board a -> [(Coordinate, Tile a)]
tileList board = concatMap iterateBoardRow [0..height board - 1]
  where iterateBoardRow r = map (\c -> (Coordinate c r, tileAt board (Coordinate c r))) [0..width board - 1]

-- | Produces a list of coordinates on the board that are ajacent (one tile away in a cardinal direction)
-- | to the given coordinate.
adjacentCoordinates :: Board a -> Coordinate -> [(Coordinate, Orientation)]
adjacentCoordinates board coordinate = filter (isOnBoard board . fst) adjacents
  where orientations = [North, East, South, West]
        offsets = map toUnitVector orientations 
        adjacents = zip (map (add coordinate) offsets) orientations

-- | Produces a list of tiles that are directly connected to the tile at the given coordinate, pair with
-- | their coordinates on the board.
connectedAdjacentTiles :: Board a -> Coordinate -> [(Coordinate, Tile a)]
connectedAdjacentTiles board coordinate = map tileOnBoard (filter isConnected $ adjacentCoordinates board coordinate)
  where fromTile = tileAt board coordinate
        isConnected (c, o) = tilesConnected fromTile (tileAt board c) o 
        tileOnBoard (coord, _) = (coord, tileAt board coord)
