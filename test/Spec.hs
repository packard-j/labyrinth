import Test.HUnit
import Tile (Tile(..), tilesConnected)
import Connector (Connector(..))
import Coordinate (Coordinate(..))
import Orientation (Orientation(..))
import Board (newBoard, tileAtSafe, reachableTiles)

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

barPlusConnected :: Test
barPlusConnected =
  tilesConnected (Tile Plus North) (Tile Bar East) East ~? 
  "─┼ should be connected" 

verticalLsConnected :: Test
verticalLsConnected =
  tilesConnected (Tile L North) (Tile  L South) North ~? 
  "┐\n└ should be connected"

verticalLsDisconnected :: Test
verticalLsDisconnected =
  not (tilesConnected (Tile L North) (Tile L South) South) ~? 
  "└\n┐ should not be connected"

lBarConnected :: Test
lBarConnected = 
  tilesConnected (Tile Bar East) (Tile L North) West ~?
  "└─ should be connected"

barLDisconnected :: Test
barLDisconnected =
  not (tilesConnected (Tile Bar East) (Tile L North) North) ~?
  "─└ should not be connected"

lTConnected :: Test
lTConnected =
  tilesConnected (Tile L South) (Tile T South) South ~?
  "┐\n┴ should be connected"

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

tests :: Test
tests = TestList
  [ "tile at (0,0)"        ~: tileAtOrigin,
    "─┼ connected"         ~: barPlusConnected,
    "┐\n└ connected"       ~: verticalLsConnected,
    "└\n┐ disconnected"    ~: verticalLsDisconnected,
    "└─ connected"         ~: lBarConnected,
    "─└ disconnected"      ~: barLDisconnected,
    "┐\n┴ connected"       ~: lTConnected,
    "reachable from (0,0)" ~: reachableFromOrigin,
    "reachable from (2,1)" ~: reachableFrom2_2,
    "reachable from (2,1)" ~: reachableFrom2_1]

main :: IO ()
main = do
  counts <- runTestTT tests
  return ()

