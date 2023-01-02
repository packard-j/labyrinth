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
  board3x2ShiftCol0D) where
import Board
import Tile
import Orientation
import Connector
import Coordinate

-- │─│
-- └┌┐
-- ┬┤┴
board3x3 :: Board
board3x3 = newBoard genTile 3 3

board3x3ShiftRow2L :: Board
board3x3ShiftRow2L = fromString
  ("│─│" ++
   "└┌┐" ++
   "┤┴┼") 3 3

board3x3ShiftCol0U :: Board
board3x3ShiftCol0U = fromString
  ("└─│" ++
   "┬┌┐" ++
   "└┤┴") 3 3

board3x3ShiftCol0UAlt :: Board
board3x3ShiftCol0UAlt = fromString
  ("└─│" ++
   "┬┌┐" ++
   "┐┤┴") 3 3

board3x2 :: Board
board3x2 = fromString
  ("┌─┐" ++
   "┼┴┤") 3 2

board3x2ShiftRow0R :: Board
board3x2ShiftRow0R = fromString
  ("│┌─" ++
   "┼┴┤") 3 2

board3x2ShiftCol0D :: Board
board3x2ShiftCol0D = fromString
  ("└─┐" ++
   "┌┴┤") 3 2

genTile :: Coordinate -> Tile
genTile (Coordinate x y) =
  Tile (connectors   !! fromIntegral (y `mod` fromIntegral (length connectors)))
       (orientations !! fromIntegral (x `mod` fromIntegral (length orientations)))
  where
    connectors   = [Bar, L, T, Plus]
    orientations = [North, East, South, West]

fromString :: String -> Integer -> Integer -> Board
fromString s width height = newBoard getTile width height where
  getTile (Coordinate x y) = tile $ s !! fromIntegral (y * width + x)
  
tile :: Char -> Tile
tile '│' = Tile Bar North
tile '─' = Tile Bar East
tile '└' = Tile L North
tile '┌' = Tile L East
tile '┐' = Tile L South
tile '┘' = Tile L West
tile '┬' = Tile T North
tile '┤' = Tile T East
tile '┴' = Tile T South
tile '├' = Tile T West
tile '┼' = Tile Plus North
tile c = error ("invalid tile: " ++ [c])

