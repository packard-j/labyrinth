module Board (newBoard, reachableTiles, tileAtSafe) where
import Tile (Tile(..), tilesConnected)
import Coordinate (Coordinate(..), add)
import Orientation (Orientation(..), toUnitVector)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.Set (Set, fromList)

data Board = Board 
  { tiles  :: [[Tile]],
    width  :: Integer,
    height :: Integer }

instance Show Board where
   show board = 
     show (width board) ++ "x" ++ show (height board) ++ " board" ++ "\n" ++
     unlines (map (unwords . map show) (tiles board))

newBoard :: (Coordinate -> Tile) -> Integer -> Integer -> Board
newBoard createTile w h =
  Board 
    (map (\row -> boardRow createTile row w) [0..h-1])
    w
    h

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

boardRow :: (Coordinate -> Tile) -> Integer -> Integer -> [Tile]
boardRow createTile row w =
  map (createTile . Coordinate row) [0..w-1]

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
