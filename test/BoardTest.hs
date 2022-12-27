module BoardTest (boardTests) where
import Test.HUnit
import Board
import Tile
import Connector
import Coordinate
import Orientation
import Data.Set (fromList)

genTile :: Coordinate -> Tile
genTile (Coordinate x y) =
  Tile (connectors   !! fromIntegral (x `mod` fromIntegral (length connectors)))
       (orientations !! fromIntegral (y `mod` fromIntegral (length orientations)))
  where
    connectors   = [Bar, L, T, Plus]
    orientations = [North, East, South, West]

tileAtOrigin :: Test
tileAtOrigin = 
  Just (Tile Bar North) ~?= tileAtSafe (newBoard genTile 3 3) (Coordinate 0 0) 

-- │ ─ │
-- └ ┌ ┐
-- ┬ ┤ ┴

reachableFromOrigin :: Test
reachableFromOrigin = 
  Just (fromList [Coordinate 0 0, Coordinate 0 1]) ~?=
  reachableTiles (newBoard genTile 3 3) (Coordinate 0 0)

reachableFrom2_2 :: Test
reachableFrom2_2 = 
  Just (fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) ~?=
  reachableTiles (newBoard genTile 3 3) (Coordinate 2 2)

reachableFrom2_1 :: Test
reachableFrom2_1 =
  Just (fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) ~?= 
  reachableTiles (newBoard genTile 3 3) (Coordinate 2 1)

boardTests :: Test
boardTests = TestList
  [ "tile at (0,0)"        ~: tileAtOrigin,
    
    "reachable from (0,0)" ~: reachableFromOrigin,
    "reachable from (2,1)" ~: reachableFrom2_2,
    "reachable from (2,1)" ~: reachableFrom2_1]
