{-# LANGUAGE TupleSections #-}
module Board (Board, newBoard, slide, reachableTiles, tileAtSafe) where
import Tile (Tile(..), tilesConnected)
import Coordinate (Coordinate(..), add)
import Orientation (Orientation(..), toUnitVector)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.Set (Set, fromList, member)
import Prelude hiding (Either(..))

data Board = Board 
  { tiles  :: [[Tile]],
    width  :: Integer,
    height :: Integer,
    movable :: Set (Axis, Integer) } deriving Eq

instance Show Board where
   show board = 
     show (width board) ++ "x" ++ show (height board) ++ " board" ++ "\n" ++
     unlines (map (concatMap show) (tiles board))

data Axis = Row | Column deriving (Eq, Ord)

newBoard :: (Coordinate -> Tile) -> Integer -> Integer -> Board
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

slide :: Board -> Tile -> Orientation -> Integer -> Maybe (Board, Tile)
slide board spare dir = slideAxis board spare (axisOf dir) dir
  where axisOf North = Row
        axisOf South = Row
        axisOf _     = Column

slideAxis :: Board -> Tile -> Axis -> Orientation -> Integer -> Maybe (Board, Tile)
slideAxis board tile axis dir index
  | index < 0 || index >= bound axis board = Nothing
  | not $ member (axis, index) (movable board) = Nothing
  | otherwise = Just (shifted dir, newSpare dir) 
  where
    shifted North = mapBoard board slideCol
    shifted South = mapBoard board slideCol
    shifted East  = mapBoard board slideRow
    shifted West  = mapBoard board slideRow
    slideRow (Coordinate x y)
      | y /= index = tileAt board (Coordinate x y)
      | x == insertAt = tile
      | otherwise = tileAt board $ Coordinate (x `op` 1) y
      where insertAt = if dir == East then 0 else width board - 1
            op       = if dir == East then (-) else (+)
    slideCol (Coordinate x y)
      | x /= index = tileAt board (Coordinate x y)
      | y == insertAt = tile
      | otherwise = tileAt board $ Coordinate x (y `op` 1)
      where insertAt = if dir == North then height board - 1 else 0
            op       = if dir == North then (+) else (-)
    newSpare North = tileAt board $ Coordinate index 0
    newSpare South = tileAt board $ Coordinate index (height board - 1)
    newSpare West  = tileAt board $ Coordinate 0 index
    newSpare East  = tileAt board $ Coordinate (width board - 1) index
    bound Row    = height
    bound Column = width

mapBoard :: Board -> (Coordinate -> Tile) -> Board
mapBoard board f = newBoard f (width board) (height board)

tileAtSafe :: Board -> Coordinate -> Maybe Tile
tileAtSafe board coordinate
  | isOnBoard board coordinate = Just $ tileAt board coordinate
  | otherwise = Nothing

reachableTiles :: Board -> Coordinate -> Maybe (Set Coordinate)
reachableTiles board coordinate = 
  let (graph, coordFromVertex, vertexFromCoord) = toGraph board in 
    do
     vertex <- vertexFromCoord coordinate
     return $ fromList (map coordFromVertex $ reachable graph vertex)

isOnBoard :: Board -> Coordinate -> Bool
isOnBoard board (Coordinate x y)
  | x < 0 || x >= width board  = False
  | y < 0 || y >= height board = False
  | otherwise                  = True

tileAt :: Board -> Coordinate -> Tile
tileAt board (Coordinate x y) = tiles board !! fromIntegral y !! fromIntegral x

toGraph :: Board -> (Graph, Vertex -> Coordinate, Coordinate -> Maybe Vertex)
toGraph board = (graph, coordFromVertex, vertexFromCoord)
  where (graph, nodeFromVertex, vertexFromCoord) = graphFromEdges $ toNodes board
        coordFromVertex v = second (nodeFromVertex v)
        second (_, x, _) = x

toNodes :: Board -> [(Tile, Coordinate, [Coordinate])]
toNodes board = map toNode $ tileList board
  where toNode (coord, tile) = (tile, coord, map fst (connectedAdjacentTiles board coord))

tileList :: Board -> [(Coordinate, Tile)]
tileList board = concatMap iterateBoardRow [0..height board - 1]
  where iterateBoardRow r = map (\c -> (Coordinate c r, tileAt board (Coordinate c r))) [0..width board - 1]

adjacentCoordinates :: Board -> Coordinate -> [(Coordinate, Orientation)]
adjacentCoordinates board coordinate = filter (isOnBoard board . fst) adjacents
  where orientations = [North, East, South, West]
        offsets = map toUnitVector orientations 
        adjacents = zip (map (add coordinate) offsets) orientations

connectedAdjacentTiles :: Board -> Coordinate -> [(Coordinate, Tile)]
connectedAdjacentTiles board coordinate = map tileOnBoard (filter isConnected $ adjacentCoordinates board coordinate)
  where fromTile = tileAt board coordinate
        isConnected (c, o) = tilesConnected fromTile (tileAt board c) o 
        tileOnBoard (coord, _) = (coord, tileAt board coord)
