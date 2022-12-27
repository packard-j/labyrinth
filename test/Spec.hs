import Test.HUnit
import Tile (Tile(..), tilesConnected)
import Connector (Connector(..))
import Coordinate (Coordinate(..))
import Orientation (Orientation(..))
import Board (newBoard, tileAtSafe, reachableTiles, toNodes)

import Data.Set (Set, fromList)

genTile :: Coordinate -> Tile
genTile (Coordinate x y) =
  Tile (connectors !! fromIntegral (x `mod` fromIntegral (length connectors)))
       (orientations !! fromIntegral (y `mod` fromIntegral (length orientations)))
  where
    connectors   = [Bar, L, T, Plus]
    orientations = [North, East, South, West]

testAccess = TestCase $
  assertEqual "tile at (0,0)" (tileAtSafe (newBoard genTile 3 3) (Coordinate 0 0)) (Just $ Tile Bar North)

testConnected = TestCase $
  assertBool "─┼ should be connected" (tilesConnected (Tile Plus North) (Tile Bar East) East)

testConnected2 = TestCase $
  assertBool "┐\n└ should be connected" (tilesConnected (Tile L North) (Tile  L South) North)

testNotConnected = TestCase $
  assertBool "└\n┐ should not be connected" (not (tilesConnected (Tile L North) (Tile L South) South))

testConnected3 = TestCase $
  assertBool "└─ should be connected" (tilesConnected (Tile Bar East) (Tile L North) West)

testNotConnected2 = TestCase $
  assertBool "─└ should not be connected" (not (tilesConnected (Tile Bar East) (Tile L North) North))

testConnectedHuh = TestCase $
  assertBool "┐\n┴ should be connected" (tilesConnected (Tile L South) (Tile T South) South)

-- │ ─ │
-- └ ┌ ┐
-- ┬ ┤ ┴

testToNodes = TestCase $
  assertEqual "nodes"
  (toNodes (newBoard genTile 3 3))
  [ (Tile Bar North,   Coordinate 0 0, [Coordinate 0 1]),
    (Tile Bar East,  Coordinate 1 0, []),
    (Tile Bar South, Coordinate 2 0, []),
    -- 
    (Tile L North,   Coordinate 0 1, [Coordinate 0 0]),
    (Tile L East,  Coordinate 1 1, [Coordinate 2 1, Coordinate 1 2]), 
    (Tile L South, Coordinate 2 1, [Coordinate 2 2, Coordinate 1 1]),
    --
    (Tile T North,   Coordinate 0 2, [Coordinate 1 2]),
    (Tile T East,  Coordinate 1 2, [Coordinate 1 1, Coordinate 0 2]),
    (Tile T South, Coordinate 2 2, [Coordinate 2 1]) ]

-- │ ─ │
-- └ ┌ ┐
-- ┬ ┤ ┴

testReachable = TestCase $
  assertEqual "tile (0, 0)" 
              (reachableTiles (newBoard genTile 3 3) (Coordinate 0 0))
              (Just $ fromList [Coordinate 0 0, Coordinate 0 1])

testReachable2 = TestCase $
  assertEqual "tile (2, 2)"
              (reachableTiles (newBoard genTile 3 3) (Coordinate 2 2))
              (Just $ fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2])

testReachable3 = TestCase $
  assertEqual "tile (2, 1)"
              (reachableTiles (newBoard genTile 3 3) (Coordinate 2 1))
              (Just $ fromList [Coordinate 1 1, Coordinate 2 1, Coordinate 0 2, Coordinate 1 2, Coordinate 2 2]) 

tests = TestList
  [ TestLabel "board access" testAccess,
    TestLabel "connected" testConnected,
    TestLabel "connected 2" testConnected2,
    TestLabel "not connected" testNotConnected,
    TestLabel "connected 3" testConnected3,
    TestLabel "not connected 2" testNotConnected2,
    TestLabel "connected huh?" testConnectedHuh,
    TestLabel "nodes" testToNodes,
    TestLabel "reachable" testReachable,
    TestLabel "reachable more" testReachable2 ]

main :: IO ()
main = do
  counts <- runTestTT tests
  return ()

