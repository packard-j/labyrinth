module Tile (Tile(..), Connector(..), Orientation(..), Coordinate(..), newBoard, tileAtSafe, reachableTiles, tilesConnected, toNodes) where
import Data.Set (Set, fromList, member)
import qualified Data.Set (map)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)

data Connector = Bar 
  | L 
  | T 
  | Plus
  deriving Eq

data Orientation = North
  | East
  | South
  | West
  deriving Eq

instance Ord Orientation where
  compare o1 o2 = compare (orientationToInt o1) (orientationToInt o2)

orientationToInt :: Orientation -> Integer
orientationToInt North   = 0
orientationToInt East  = 1
orientationToInt South = 2
orientationToInt West = 3

intToOrientation :: Integer -> Orientation
intToOrientation i = [North, East, South, West] !! (fromIntegral i `mod` 4)

sides :: Connector -> Set Orientation
sides Bar  = fromList [North, South             ]
sides L    = fromList [North, East              ]
sides T    = fromList [East,  South, West       ]
sides Plus = fromList [North, East,  South, West]

rotateClockwiseBy :: Orientation -> Orientation -> Orientation
rotateClockwiseBy orientation by = intToOrientation (sum $ map orientationToInt [orientation, by])

data Tile = Tile Connector Orientation
  deriving Eq

instance Show Tile where
  show (Tile Bar  North) = "│"
  show (Tile Bar  East)  = "─"
  show (Tile Bar  South) = "│"
  show (Tile Bar  West)  = "─"
  show (Tile L    North) = "└"
  show (Tile L    East)  = "┌"
  show (Tile L    South) = "┐"
  show (Tile L    West)  = "┘"
  show (Tile T    North) = "┬"
  show (Tile T    East)  = "┤"
  show (Tile T    South) = "┴"
  show (Tile T    West)  = "├"
  show (Tile Plus _)     = "┼"

rotateTileClockwiseBy :: Tile -> Orientation -> Tile
rotateTileClockwiseBy (Tile connector orientation) by = Tile connector $ rotateClockwiseBy orientation by

tileConnections :: Tile -> Set Orientation
tileConnections (Tile connector orientation) = 
  Data.Set.map (rotateClockwiseBy orientation) (sides connector) 

tilesConnectedVertical :: Tile -> Tile -> Bool
tilesConnectedVertical bottom top = bottomHasDeg0 && topHasDeg180
  where bottomHasDeg0 = member North   $ tileConnections bottom 
        topHasDeg180  = member South $ tileConnections top

tilesConnected :: Tile -> Tile -> Orientation -> Bool
tilesConnected t1 t2 North = tilesConnectedVertical t1 t2
tilesConnected t1 t2 East  = tilesConnectedVertical (rotateTileClockwiseBy t1 West)
                                                    (rotateTileClockwiseBy t2 West)
tilesConnected t1 t2 South = tilesConnectedVertical t2 t1
tilesConnected t1 t2 West  = tilesConnectedVertical (rotateTileClockwiseBy t1 East)
                                                    (rotateTileClockwiseBy t2 East)

data Coordinate = Coordinate Integer Integer deriving Eq

instance Show Coordinate where
  show (Coordinate x y) = show (x,y)

instance Ord Coordinate where
  compare (Coordinate x1 y1) (Coordinate x2 y2)
    | x1 < x2              = LT
    | x1 == x2 && y1 < y2  = LT
    | x1 == x2 && y1 == y2 = EQ
    | otherwise            = GT

add :: Coordinate -> Coordinate -> Coordinate
add (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)

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

toGraph :: Board -> (Graph, Vertex -> Coordinate, Coordinate -> Maybe Vertex)
toGraph board = (graph, coordFromVertex, vertexFromCoord)
  where (graph, nodeFromVertex, vertexFromCoord) = graphFromEdges $ toNodes board
        coordFromVertex v = third (nodeFromVertex v)

third :: (a, b, c) -> b
third (_, b, _) = b

toNodes :: Board -> [(Tile, Coordinate, [Coordinate])]
toNodes board = map toNode $ iterateBoard board
  where toNode (coord, tile) = (tile, coord, map fst (connectedAdjacentTiles board coord))

iterateBoard :: Board -> [(Coordinate, Tile)]
iterateBoard board = concatMap iterateBoardRow [0..height board - 1]
  where iterateBoardRow r = map (\c -> (Coordinate c r, tileAt board (Coordinate c r))) [0..width board - 1]

adjacentCoordinates :: Board -> Coordinate -> [(Coordinate, Orientation)]
adjacentCoordinates board coordinate = filter (isOnBoard board . fst) adjacents
  where orientations = [North, East, South, West]
        offsets = map toCoordOffset orientations 
        adjacents = zip (map (add coordinate) offsets) orientations

toCoordOffset :: Orientation -> Coordinate
toCoordOffset North = Coordinate 0  (-1)
toCoordOffset East  = Coordinate 1    0
toCoordOffset South = Coordinate 0    1
toCoordOffset West  = Coordinate (-1) 0

connectedAdjacentTiles :: Board -> Coordinate -> [(Coordinate, Tile)]
connectedAdjacentTiles board coordinate = map tileOnBoard (filter isConnected $ adjacentCoordinates board coordinate)
  where fromTile = tileAt board coordinate
        isConnected (c, o) = tilesConnected fromTile (tileAt board c) o 
        tileOnBoard (coord, _) = (coord, tileAt board coord)
