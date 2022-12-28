module BoardTest (boardTests) where
import Test.HUnit
import Board
import Tile
import Connector
import Coordinate
import Orientation
import Data.Set (fromList)
import Control.Monad.Zip (munzip)

genTile :: Coordinate -> Tile
genTile (Coordinate x y) =
  Tile (connectors   !! fromIntegral (y `mod` fromIntegral (length connectors)))
       (orientations !! fromIntegral (x `mod` fromIntegral (length orientations)))
  where
    connectors   = [Bar, L, T, Plus]
    orientations = [North, East, South, West]

fromString :: String -> Integer -> Integer -> Board
fromString s width height = newBoard getTile width height where
  getTile (Coordinate x y) = toTile $ s !! fromIntegral (y * height + x)
  toTile '│' = Tile Bar North
  toTile '─' = Tile Bar East
  toTile '└' = Tile L North
  toTile '┌' = Tile L East
  toTile '┐' = Tile L South
  toTile '┘' = Tile L West
  toTile '┬' = Tile T North
  toTile '┤' = Tile T East
  toTile '┴' = Tile T South
  toTile '├' = Tile T West
  toTile '┼' = Tile Plus North
  toTile c = error ("invalid tile: " ++ [c])

-- │ ─ │
-- └ ┌ ┐
-- ┬ ┤ ┴
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

tileAtOrigin :: Test
tileAtOrigin = 
  Just (Tile Bar North) ~=? tileAtSafe board3x3 (Coordinate 0 0) 

tileAt1_2 :: Test
tileAt1_2 =
  Just (Tile T East) ~=? tileAtSafe board3x3 (Coordinate 1 2)

slideRow2Left :: Test
slideRow2Left = TestList
  [ Just (Tile T North)     ~=? newSpare,
    Just board3x3ShiftRow2L ~=? board ] where
  (board, newSpare) = munzip $ slide board3x3 (Tile Plus North) West 2

slideCol0Up :: Test
slideCol0Up = TestList
  [ Just (Tile Bar North) ~=? newSpare,
    Just board3x3ShiftCol0U ~=? board ] where
  (board, newSpare) = munzip $ slide board3x3 (Tile L North) North 0

slideImmovableCol :: Test
slideImmovableCol = Nothing ~=? slide board3x3 (Tile T East) North 1

slideImmovableRow :: Test
slideImmovableRow = Nothing ~=? slide board3x3 (Tile L South) East 1

reachableFromOrigin :: Test
reachableFromOrigin = 
  Just (fromList [Coordinate 0 0, Coordinate 0 1]) ~=?
  reachableTiles board3x3 (Coordinate 0 0)

reachableFrom2_2 :: Test
reachableFrom2_2 = 
  Just (fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) ~=?
  reachableTiles board3x3 (Coordinate 2 2)

reachableFrom2_1 :: Test
reachableFrom2_1 =
  Just (fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) ~=? 
  reachableTiles board3x3 (Coordinate 2 1)

boardTests :: Test
boardTests = TestList
  [ "tile at (0,0)"        ~: tileAtOrigin,
    "tile at (1,2)"        ~: tileAt1_2,
    "slide row 2 left"     ~: slideRow2Left,
    "slide col 0 up"       ~: slideCol0Up,
    "slide immovable row"  ~: slideImmovableRow,
    "slide immovable col"  ~: slideImmovableCol,
    "reachable from (0,0)" ~: reachableFromOrigin,
    "reachable from (2,1)" ~: reachableFrom2_2,
    "reachable from (2,1)" ~: reachableFrom2_1]
