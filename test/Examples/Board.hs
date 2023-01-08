module Examples.Board 
 (genTile,
  fromString,
  tile,
  board3x3,
  board3x3ShiftRow2L,
  board3x3ShiftCol0U,
  board3x3ShiftCol0UAlt,
  board3x2,
  board3x2ShiftRow0R,
  board3x2ShiftCol0D,
  board7x7,
  genTreasures) where
import Data.JSON.Board (gemNames)
import Maze.Board
import Maze.Tile
import Maze.Orientation
import Maze.Connector
import Maze.Coordinate
import Data.Set (toList)

-- │─│
-- └┌┐
-- ┬┤┴
board3x3 :: Board ()
board3x3 = newBoard genTile 3 3

board3x3ShiftRow2L :: Board ()
board3x3ShiftRow2L = fromString
  ("│─│" ++
   "└┌┐" ++
   "┤┴┼") 3 3

board3x3ShiftCol0U :: Board ()
board3x3ShiftCol0U = fromString
  ("└─│" ++
   "┬┌┐" ++
   "└┤┴") 3 3

board3x3ShiftCol0UAlt :: Board ()
board3x3ShiftCol0UAlt = fromString
  ("└─│" ++
   "┬┌┐" ++
   "┐┤┴") 3 3

board3x2 :: Board ()
board3x2 = fromString
  ("┌─┐" ++
   "┼┴┤") 3 2

board3x2ShiftRow0R :: Board ()
board3x2ShiftRow0R = fromString
  ("│┌─" ++
   "┼┴┤") 3 2

board3x2ShiftCol0D :: Board ()
board3x2ShiftCol0D = fromString
  ("└─┐" ++
   "┌┴┤") 3 2

board7x7 :: Board (String, String)
board7x7 = fromStringWithTreasures (
 "│─┐└┌┘┬" ++
 "│─┐└┌┘┬" ++
 "│─┐└┌┘┬" ++
 "│─┐└┌┘┬" ++
 "│─┐└┌┘┬" ++
 "│─┐└┌┘┬" ++
 "│─┐└┌┘┬") 
 (genTreasures rows cols) rows cols where
   rows = 7
   cols = 7

genTile :: Coordinate -> Tile ()
genTile (Coordinate x y) =
  Tile (connectors   !! fromIntegral (y `mod` fromIntegral (length connectors)))
       (orientations !! fromIntegral (x `mod` fromIntegral (length orientations)))
       ()
  where
    connectors   = [Bar, L, T, Plus]
    orientations = [North, East, South, West]

genTreasures :: Int -> Int -> [[(String, String)]]
genTreasures rows columns = chunks rows columns treasures where
  chunks n chunkSize l
   | n == 0    = []
   | otherwise = take chunkSize l:chunks (n-1) chunkSize (drop chunkSize l)
  treasures = (,) <$> gems <*> gems
  gems = toList gemNames

fromString :: String -> Int -> Int -> Board ()
fromString s width height = fromStringWithTreasures s treasures width height where
  treasures = replicate height (replicate width ())

fromStringWithTreasures :: String -> [[a]] -> Int -> Int -> Board a
fromStringWithTreasures s t width height = newBoard getTile cols rows where
  cols = fromIntegral width
  rows = fromIntegral height
  getTile (Coordinate x y) = tileWith (s !! fromIntegral (y * cols + x))
                                      (t !! fromIntegral y !! fromIntegral x)

tileWith :: Char -> a -> Tile a
tileWith '│' = Tile Bar North
tileWith '─' = Tile Bar East
tileWith '└' = Tile L North
tileWith '┌' = Tile L East
tileWith '┐' = Tile L South
tileWith '┘' = Tile L West
tileWith '┬' = Tile T North
tileWith '┤' = Tile T East
tileWith '┴' = Tile T South
tileWith '├' = Tile T West
tileWith '┼' = Tile Plus North
tileWith c = error ("invalid tile: " ++ [c])
  
tile :: Char -> Tile ()
tile c = tileWith c ()
