module BoardTest (boardTests) where
import Test.HUnit
import Examples.Board
import Maze.Board
import Maze.Tile
import Maze.Coordinate
import Maze.Orientation
import Data.Set (fromList)
import Control.Monad.Zip (munzip)
import Control.Monad.Except

-- | Perform N successive slides, each one using the spare tile from the previous
slideN :: Board a -> Tile a -> [(Orientation, Integer)] -> BoardResult (Board a, Tile a)
slideN board spare = foldM slideOne (board, spare) where
  slideOne (b, s) (dir, index) = slide b s dir index

tileAtOrigin :: Test
tileAtOrigin = 
  return (tile '│') ~=? tileAtSafe board3x3 (Coordinate 0 0) 

tileAt1_2 :: Test
tileAt1_2 =
  return (tile '┤') ~=? tileAtSafe board3x3 (Coordinate 1 2)

slideRow2Left :: Test
slideRow2Left = TestList
  [ return (tile '┬')         ~=? newSpare,
    return board3x3ShiftRow2L ~=? board ] where
  (board, newSpare) = munzip $ slide board3x3 (tile '┼') West 2

slideCol0Up :: Test
slideCol0Up = TestList
  [ return (tile '│')   ~=? newSpare,
    return board3x3ShiftCol0U ~=? board ] where
  (board, newSpare) = munzip $ slide board3x3 (tile '└') North 0

slideRow0Right :: Test
slideRow0Right = TestList
  [ return (tile '┐')     ~=? newSpare,
    return board3x2ShiftRow0R ~=? board ] where
  (board, newSpare) = munzip $ slide board3x2 (tile '│') East 0

slideCol0Down :: Test
slideCol0Down = TestList
  [ return (tile '┼')       ~=? newSpare,
    return board3x2ShiftCol0D ~=? board ] where
  (board, newSpare) = munzip $ slide board3x2 (tile '└') South 0

unslide :: Test
unslide = TestList
  [ return (tile '┼') ~=? spare,
    return board3x3   ~=? board ] where
  (board, spare) = munzip $ slideN board3x3 (tile '┼') [(North, 0), (South, 0), (East, 2), (West, 2)]

slideImmovableCol :: Test
slideImmovableCol = throwError (Immovable (Column, 1)) ~=? slide board3x3 (tile '┤') North 1

slideImmovableRow :: Test
slideImmovableRow = throwError (Immovable (Row, 1)) ~=? slide board3x3 (tile '┐') East 1

reachableFromOrigin :: Test
reachableFromOrigin = 
  return (fromList [Coordinate 0 0, Coordinate 0 1]) ~=?
  reachableTiles board3x3 (Coordinate 0 0)

reachableFrom2_2 :: Test
reachableFrom2_2 = 
  return (fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) ~=?
  reachableTiles board3x3 (Coordinate 2 2)

reachableFrom2_1 :: Test
reachableFrom2_1 =
  return (fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) ~=? 
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
