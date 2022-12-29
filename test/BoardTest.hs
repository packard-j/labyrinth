module BoardTest (boardTests) where
import Test.HUnit
import Board
import Tile
import Connector
import Coordinate
import Orientation
import Data.Set (fromList)
import Control.Monad (foldM)
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

-- | Perform N successive slides, each one using the spare tile from the previous
slideN :: Board -> Tile -> [(Orientation, Integer)] -> Maybe (Board, Tile)
slideN board spare = foldM slideOne (board, spare) where
  slideOne (b, s) (dir, index) = slide b s dir index

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

tileAtOrigin :: Test
tileAtOrigin = 
  Just (tile '│') ~=? tileAtSafe board3x3 (Coordinate 0 0) 

tileAt1_2 :: Test
tileAt1_2 =
  Just (tile '┤') ~=? tileAtSafe board3x3 (Coordinate 1 2)

slideRow2Left :: Test
slideRow2Left = TestList
  [ Just (tile '┬')         ~=? newSpare,
    Just board3x3ShiftRow2L ~=? board ] where
  (board, newSpare) = munzip $ slide board3x3 (tile '┼') West 2

slideCol0Up :: Test
slideCol0Up = TestList
  [ Just (tile '│')   ~=? newSpare,
    Just board3x3ShiftCol0U ~=? board ] where
  (board, newSpare) = munzip $ slide board3x3 (tile '└') North 0

slideRow0Right :: Test
slideRow0Right = TestList
  [ Just (tile '┐')     ~=? newSpare,
    Just board3x2ShiftRow0R ~=? board ] where
  (board, newSpare) = munzip $ slide board3x2 (tile '│') East 0

slideCol0Down :: Test
slideCol0Down = TestList
  [ Just (tile '┼')       ~=? newSpare,
    Just board3x2ShiftCol0D ~=? board ] where
  (board, newSpare) = munzip $ slide board3x2 (tile '└') South 0

unslide :: Test
unslide = TestList
  [ Just (tile '┼') ~=? spare,
    Just board3x3   ~=? board ] where
  (board, spare) = munzip $ slideN board3x3 (tile '┼') [(North, 0), (South, 0), (East, 2), (West, 2)]

slideImmovableCol :: Test
slideImmovableCol = Nothing ~=? slide board3x3 (tile '┤') North 1

slideImmovableRow :: Test
slideImmovableRow = Nothing ~=? slide board3x3 (tile '┐') East 1

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
    "slide row 0 right"    ~: slideRow0Right,
    "slide col 0 down"     ~: slideCol0Down,
    "slide immovable row"  ~: slideImmovableRow,
    "slide immovable col"  ~: slideImmovableCol,
    "unslide"              ~: unslide,
    "reachable from (0,0)" ~: reachableFromOrigin,
    "reachable from (2,1)" ~: reachableFrom2_2,
    "reachable from (2,1)" ~: reachableFrom2_1]
